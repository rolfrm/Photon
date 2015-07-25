#include <stdio.h>
#include <iron/full.h>
#include "lisp_parser.h"
#include <libtcc.h>
#include "lisp_types.h"
#include "c_ast.h"
#include "lisp_std_types.h"
#include "lisp_compiler.h"
#include "type_pool.h"
#include "expr_utils.h"
#include "builtin_macros.h"
#include "builtin_functions.h"

var_def * get_any_variable(symbol s){
  var_def * stackvar = get_stack_variable(s);
  if(stackvar == NULL)
    stackvar = get_global(s);
  return stackvar;
}
	  
type_def * compile_value(c_value * val, value_expr e){
  val->type = C_INLINE_VALUE;
  var_def * vdef = NULL;
  switch(e.type){
  case STRING:
    {
      char * chr = fmtstr("%.*s",e.strln,e.value);
      symbol s = get_symbol(chr);
      dealloc(chr);
      chr = symbol_name(s);
      char buf[100];
      sprintf(buf, "__istr_%i", s.id);
      symbol bufsym = get_symbol(buf);
      define_variable(bufsym, type_pool_get(&char_ptr_def), chr, false);// clone(&chr, sizeof(chr)));
      val->type = C_SYMBOL;
      val->symbol = bufsym;
      return type_pool_get(&char_ptr_def);
    }
  case KEYWORD:
  case SYMBOL:
    val->type = C_SYMBOL;
    val->symbol = vexpr_symbol(e);
    vdef = get_any_variable(val->symbol);
    if(vdef == NULL){
      COMPILE_ERROR("Unknown variable '%s'", symbol_name(val->symbol));
    }
    return type_pool_get(vdef->type);
  case NUMBER:
    val->raw.value = fmtstr("%.*s",e.strln,e.value);
    bool isfloat = false;
    for(u32 i = 0; i < e.strln;i++){
      if(e.value[i] == '.'){
	isfloat = true;
	break;
      }
    }
    type_def *t  = isfloat ? &f64_def: &i64_def;
    val->raw.type = t;
    return t;
    break;
  case COMMENT:
    break;
  }
  logd("type: %i\n", e.type);
  COMPILE_ERROR(false);
}

type_def * expr2type(expr typexpr){
  if(typexpr.type == EXPR){
    sub_expr sexp = typexpr.sub_expr;
    COMPILE_ASSERT(sexp.cnt > 0);
    expr kind = sexp.exprs[0];
    COMPILE_ASSERT(kind.type == VALUE && kind.value.type == SYMBOL);
    value_expr vkind = kind.value;

    if(strncmp(vkind.value,"fcn",vkind.strln) == 0){
      type_def out;
      out.type = FUNCTION;
      COMPILE_ASSERT(sexp.cnt > 1);
      type_def * ret = expr2type(sexp.exprs[1]);
    
      COMPILE_ASSERT(&error_def != ret);
      type_def * args[sexp.cnt - 2];
      for(size_t i = 0; i < sexp.cnt - 2; i++){
	expr arg = sexp.exprs[i + 2];
	COMPILE_ASSERT(arg.type == EXPR && arg.sub_expr.cnt == 2 && is_symbol(arg.sub_expr.exprs[0]));
	args[i] = expr2type(arg.sub_expr.exprs[1]);
	COMPILE_ASSERT(args[i] != NULL && args[i] != &error_def);
      } 
      out.fcn.ret = ret;
      out.fcn.args = args;
      out.fcn.cnt = array_count(args);
      return type_pool_get(&out);

    }else if (strncmp(vkind.value,"ptr",vkind.strln) == 0){
      COMPILE_ASSERT(sexp.cnt == 2);
      type_def out;
      out.type = POINTER;
      out.ptr.inner = expr2type(sexp.exprs[1]);
      return type_pool_get(&out);
    }else if (strncmp(vkind.value, "struct",vkind.strln) == 0){
      COMPILE_ASSERT(sexp.cnt >= 2);
      symbol name = symbol_empty;
      bool is_anon = sexp.exprs[1].type == EXPR;
      if(!is_anon){
	COMPILE_ASSERT(sexp.exprs[1].value.type == SYMBOL);
	name = vexpr_symbol(sexp.exprs[1].value);
      }
      size_t memcnt = sexp.cnt - 1 - (is_anon ? 0 : 1);
      expr * sub = sexp.exprs + 1 + (is_anon ? 0 : 1);
      decl members[memcnt];
      for(size_t i = 0 ; i < memcnt; i++){
	COMPILE_ASSERT(sub->type == EXPR && sub->sub_expr.cnt == 2);
	sub_expr sx = sub->sub_expr;
	COMPILE_ASSERT(sx.exprs[0].type == VALUE && sx.exprs[0].value.type == SYMBOL);
	members[i].name = vexpr_symbol(sx.exprs[0].value);
	members[i].type = expr2type(sx.exprs[1]);
	sub++;
      }
      type_def out;
      out.type = STRUCT;
      out.cstruct.members = clone(members,sizeof(members));
      out.cstruct.cnt = memcnt;
      out.cstruct.name = name;
      return type_pool_get(&out);
    }else if(strncmp(vkind.value, "opaque-struct", vkind.strln) == 0){
      COMPILE_ASSERT(sexp.cnt == 2);
      type_def out;
      out.type = OPAQUE_STRUCT;
      out.cstruct.members = NULL;
      out.cstruct.cnt = 0;
      out.cstruct.name = vexpr_symbol(sexp.exprs[1].value);
      return type_pool_get(&out);
    }else if (strncmp(vkind.value, "alias", vkind.strln) == 0){
      COMPILE_ASSERT(sexp.cnt == 3);
      COMPILE_ASSERT(is_symbol(sexp.exprs[2]));
      type_def out;
      out.type = TYPEDEF;
      out.ctypedef.name = vexpr_symbol(sexp.exprs[2].value);
      out.ctypedef.inner = expr2type(sexp.exprs[1]);
      return type_pool_get(&out);
    }
  }else{
    type_def * td = type_pool_simple(expr_symbol(typexpr));
    if(td != NULL && td != &error_def) return td;
  }
  loge("Unable to understand type: ");
  print_expr(&typexpr);
  logd("\n");
  return &error_def;
}

bool is_number_literal(expr ex){
  return ex.type == VALUE && ex.value.type == NUMBER;
}
bool is_float_literal(expr ex){
  if(is_number_literal(ex)){
    for(size_t i = 0; i < ex.value.strln; i++){
      if(ex.value.value[i] == '.'){
	return true;
      }
    }
  }
  return false;
}

bool is_type_compatible(type_def * call_type, type_def * arg_type, expr callexpr){
  if(call_type == arg_type)
    return true;
  bool is_number = is_number_literal(callexpr);
  bool is_float = is_float_literal(callexpr);
  bool is_integral = is_number && !is_float;
  if(is_float_type(arg_type)&& is_number){
    return true;
  }else if(is_integral && is_integral_type(arg_type)){
    return true;
  }
  return false;	   
}

type_def * _compile_expr(c_block * block, c_value * value, sub_expr * se){
  COMPILE_ASSERT(se->cnt > 0);
  expr name_expr = se->exprs[0];
  if(name_expr.type != VALUE){
    c_value * tmpvar = alloc0(sizeof(c_value));
    type_def * td = compile_expr(block, tmpvar, name_expr); 

    if(td->type == FUNCTION || (td->type == POINTER && td->ptr.inner->type == FUNCTION)){
      // So the name expression returns a function pointer or function.
      // This can be called, by setting it to a temp variable and calling that in C.

      c_var var;
      expr * sym = gensym();
      symbol s = expr_symbol(*sym);
      {
	var.value = tmpvar;
	var.var.name = s;
	var.var.type = td;
	c_expr var_expr;
	var_expr.type = C_VAR;
	var_expr.var = var;
	block_add(block, var_expr);
      }
      expr sexprs[se->cnt];
      sexprs[0] = *sym;
      for(size_t i = 1; i < se->cnt;i++)
	sexprs[i] = se->exprs[i];
      sub_expr se2;
      se2.cnt = se->cnt;
      se2.exprs = sexprs;

      var_def * vardef = alloc0(sizeof(var_def));
      vardef->name = s;
      vardef->type = td;
      vardef->data = NULL;
      expr e;
      e.type = EXPR;
      e.sub_expr = se2;
      size_t cnt = 1;
      push_symbols(&vardef, &cnt);
      type_def * td2 = compile_expr(block,value,e);
      pop_symbols();
      return td2;
    }else{
      loge("Unable to call type:\n");
      print_decl(td, get_symbol("f"));
      logd("\n");
      return &error_def;
    }
  }
  if(name_expr.value.type != SYMBOL){
    loge("Cannot call expr ");
    print_expr(&name_expr);
    logd("\n");
    return &error_def;
  }
  
  expr * args = se->exprs + 1;
  i64 argcnt = se->cnt - 1;

  symbol name = expr_symbol(name_expr);
  ASSERT(name.id != 0);
  void * var_data;
  type_def * var_type;
  {
    var_def * fvar = get_any_variable(name);
    if(fvar == NULL) COMPILE_ERROR("unknown symbol '%s'", symbol_name(name));
    var_data = fvar->data;
    var_type = fvar->type;
  }
  
  if(var_type == type_pool_get(&cmacro_def_def)){
    cmacro_def * macro = var_data;

    if(macro->arg_cnt != argcnt && macro->arg_cnt != -1)
      COMPILE_ERROR("Unsupported number of arguments %i for %s",argcnt, macro->name);
    type_def * ( *macro_fcn)(c_block * block, c_value * val, ...) = macro->fcn;

    switch(macro->arg_cnt){
    case 0:
      return macro_fcn(block, value);
    case 1:
      return macro_fcn(block, value, args[0]);
    case 2:
      return macro_fcn(block, value, args[0], args[1]);
    case 3:
      return macro_fcn(block, value, args[0], args[1], args[2]);
    case 4:
      return macro_fcn(block, value, args[0], args[1], args[2], args[3]);
    case 5:
      return macro_fcn(block, value, args[0], args[1], args[2], args[3], args[4]);
    case 6:
      return macro_fcn(block, value, args[0], args[1], args[2], args[3], args[4], args[5]);
    case -1:
      return ((type_def *(*)(c_block * block, c_value * value, expr *,size_t))macro->fcn)
	(block,value,args, argcnt);
    default:
      ERROR("Number of macro arguments not supported: %i", argcnt);
    }
  }else if(var_type->type == FUNCTION || 
	   (var_type->type == POINTER && var_type->ptr.inner->type == FUNCTION)){
    type_def * td = var_type->type == POINTER ? var_type->ptr.inner : var_type;

    if(td->fcn.cnt != argcnt){
      COMPILE_ERROR("Too few arguments to function '%s' got %i expected %i.", symbol_name(name), argcnt, td->fcn.cnt);
    }
    c_value fargs[argcnt];
    memset(fargs,0,sizeof(c_value) * argcnt);
    type_def * farg_types[argcnt];
    int err_arg = -1;
    for(i64 i = 0; i < argcnt; i++){
      farg_types[i] = compile_expr(block, fargs + i, args[i]);
      if(!is_type_compatible(farg_types[i],td->fcn.args[i], args[i])){
	char buf[10];
	sprintf(buf, "arg%i", i);
	symbol fcnsym = get_symbol(buf);
	err_arg = i;
	loge("Error at argument %i of call to '%s'.", i, symbol_name(name));
	logd("\nExpected: ");
	print_decl(td->fcn.args[i], fcnsym);
	logd("\nGot     : ");
	print_decl(farg_types[i], fcnsym);
	logd("\n\n");
	break;
      }
    }
    if(err_arg >= 0)
      COMPILE_ERROR("Non matching types for function '%s' arg %i\n", symbol_name(name), err_arg);
    

    c_function_call call;
    call.type = td;
    call.name = name;
    call.args = clone(fargs,sizeof(fargs));
    call.arg_cnt = argcnt;

    value->type = C_FUNCTION_CALL;
    value->call = call;   
    return td->fcn.ret;
  }else if(var_type == macro_store_type()){
    type_def * _t = expand_macro(block, value, se->exprs, se->cnt);
    if(_t == &error_def){
      COMPILE_ERROR("Caught error while expanding macro '%s'\n", symbol_name(name));
    }
    return _t;
  }else{

    ERROR("Not supported.. %i\n", var_type->type);
  }
  return &error_def;
}
	  
type_def * compile_expr(c_block * block, c_value * val,  expr e ){
  type_def * td;
  switch(e.type){
  case EXPR:
    return _compile_expr(block, val, &e.sub_expr);
    break;
  case VALUE:
    td = compile_value(val,e.value);
    break;
  case ERROR:
    return &error_def;
  }	  
  
  return td;
}

c_root_code compile_lisp_to_eval(expr exp){

  c_root_code r;
  c_fcndef * f = &r.fcndef;
  r.type = C_FUNCTION_DEF;

  f->block.expr_cnt = 0;
  f->block.exprs = NULL; 
  c_value val;
  val.type = 0;
  type_def * t = compile_expr(&f->block, &val, exp);
  type_def td;
  td.type = FUNCTION;
  td.fcn.ret = t;
  td.fcn.args = NULL;
  td.fcn.cnt = 0;

  f->name = get_symbol("eval");
  f->type = type_pool_get(&td);

  c_expr expr;
  expr.type = C_VALUE;
  if(t != type_pool_get(&void_def)) expr.type = C_RETURN;
  expr.value = val;
  list_add((void **) &f->block.exprs, &f->block.expr_cnt, &expr, sizeof(c_expr));

  return r;
}

type_def * str2type(char * str){
  return expr2type(lisp_parse1(str));
}

#include <libtcc.h>
#include <stdlib.h>

char *getcwd(char *buf, size_t size);

char * orig_dir = NULL;

char * get_orig_dir(){
  if(orig_dir == NULL){
    static char dirbuf[100];
    getcwd(dirbuf,array_count(dirbuf));
    orig_dir = dirbuf;
  }
  return orig_dir;
}

void tccerror(void * opaque, const char * msg){
  UNUSED(opaque);
  loge("TCC error: %s\n",msg);
}

TCCState * mktccs(){ 
  TCCState * tccs = tcc_new();
  tcc_set_lib_path(tccs, get_orig_dir());
  tcc_set_error_func(tccs, NULL, tccerror);
  tcc_set_output_type(tccs, TCC_OUTPUT_MEMORY);
  return tccs;
}


void go_write(type_def ** deps, symbol * vdeps, c_root_code * codes, size_t code_cnt){
  
  write_dependencies(deps);
  for(size_t i = 0; vdeps[i].id != 0; i++){
    var_def * var = get_global(vdeps[i]);
    if(var == NULL){
      ERROR("No global variable named '%s'", symbol_name(vdeps[i]));
      continue;
    }

    type_def * t = var->type;
    while(t->type == POINTER){
      t = t->ptr.inner;
    }

   
    decl dcl;
    dcl.name = var->name;
    dcl.type = var->type;
    format("extern ");
    print_cdecl(dcl);format(";\n");
  }
  
  for(size_t i = 0; i < code_cnt; i++)
    print_c_code(codes[i]);
  
}

void checkvdeps(symbol * vdep){
  int it = 0;
  while(vdep[it].id != 0){
    if(symbol_name(*vdep) == NULL){
      ERROR("Symbol NULL %i", it);
    }
    if(type_pool_simple(*vdep) != NULL){
      ERROR("Name '%s' already used as type. This usecase currently not supported.\n", symbol_name(vdep[it]));
    }
    it++;
  }
}

void * compile_as_c(c_root_code * codes, size_t code_cnt){
  type_def * deps[1000];
  symbol vdeps[1000];
  memset(deps, 0, sizeof(deps));
  memset(vdeps, 0, sizeof(vdeps));
  for(size_t i = 0; i < code_cnt; i++)
    c_root_code_dep(deps, vdeps, codes[i]);
  
  checkvdeps(vdeps);
  char * data = NULL;
  size_t cnt = 0;
  FILE * f = open_memstream(&data, &cnt);
  push_format_out(f);
  go_write(deps, vdeps, codes, code_cnt);
  pop_format_out();
  fclose(f);
  char header[] = "//***********\n";
  char compile_out_path[1000];
  sprintf(compile_out_path,"%s/%s",get_orig_dir(), "compile_out.c");
  append_buffer_to_file(header,sizeof(header) - 1, compile_out_path);
  append_buffer_to_file(data,cnt,compile_out_path);
  TCCState * tccs = mktccs();
  for(size_t i = 0; i < array_count(vdeps) && vdeps[i].id != 0; i++){
    var_def * var = get_global(vdeps[i]);
    ASSERT(var != NULL);
    int fail = 0;
    if(var->is_ptr)
      fail = tcc_add_symbol(tccs,get_c_name(var->name),var->data);
    else
      fail = tcc_add_symbol(tccs,get_c_name(var->name),&var->data);
    ASSERT(!fail);
  }
  
  int fail = tcc_compile_string(tccs, data);
  free(data);
  ASSERT(!fail);
  data = NULL;
  int size = tcc_relocate(tccs, NULL);
  void * code_buffer = alloc(size);
  fail = tcc_relocate(tccs, code_buffer);
  ASSERT(!fail);
  
  for(size_t i = 0; i < code_cnt; i++){
    c_root_code r = codes[i];
    if(r.type == C_FUNCTION_DEF){
      void * ptr = tcc_get_symbol(tccs, get_c_name(r.fcndef.name));
      ASSERT(ptr != NULL);
      define_variable(r.fcndef.name, r.fcndef.type, ptr, true);
    }else if(r.type == C_VAR_DEF){

      decl vdecl = r.var.var;
      void * ptr = tcc_get_symbol(tccs, get_c_name(vdecl.name));
      ASSERT(ptr != NULL);
      define_variable(vdecl.name, vdecl.type, ptr, true);
    }
  }

  tcc_delete(tccs);
  return code_buffer;
}

void lisp_load_base(){
  // Types
  load_defs();

  // Macros
  builtin_macros_load();

  // Functions
  load_functions();

}

size_t delete_soon_cnt = 0;
void ** delete_soon_ptrs = NULL;

void add_delete_soon(void * buffer){
  list_add((void **) &delete_soon_ptrs, &delete_soon_cnt, &buffer, sizeof(void *));
}

void run_delete(){
  for(size_t i = 0; i < delete_soon_cnt; i++){
    dealloc(delete_soon_ptrs[i]);
  }
  list_clean((void **) &delete_soon_ptrs, &delete_soon_cnt);
}

allocator * current_allocator = NULL;
void print_current_mem(int id){
  if(current_allocator != NULL)
    logd("MEM%i: %i\n", id, trace_allocator_allocated_pointers(current_allocator));
}
var_def * lispcompile_expr(expr ex, compile_status * optout_status){
  //allocator * trace_alloc = trace_allocator_make();
  with_allocator(/*trace_alloc*/ NULL, lambda(void,(){
	//allocator * prev = current_allocator;
	//current_allocator = trace_alloc;
	c_root_code cl = compile_lisp_to_eval(ex);
	//print_current_mem(1);
	if(cl.fcndef.type->fcn.ret == &error_def){
	  if(optout_status != NULL)*optout_status = COMPILE_ERROR;
	  return;
	}
	
	void * codebuf = compile_as_c(&cl,1);
	
	//print_current_mem(2);
	c_root_code_delete(cl);	
	
	//print_current_mem(3);
	
	add_delete_soon(codebuf);
	//current_allocator = prev;
	//logd("Will delete: %i\n", delete_soon_cnt);
      }));
  if(*optout_status == COMPILE_ERROR)
    return NULL;
  return get_global(get_symbol("eval"));
}

void * lisp_compile_and_run_expr(expr ex, compile_status * optout_status){
  compile_status _status;
  if(optout_status == NULL) optout_status = &_status;
       
  var_def * var = lispcompile_expr(ex, optout_status);
  if(COMPILE_ERROR == *optout_status)
    return NULL;
  void * (*fcn)() = var->data;
  
  ASSERT(fcn != NULL);
  void * r = fcn();
  return r;
}

symbol * printer = NULL;

compile_status lisp_run_expr(expr ex){
  run_delete();
  expr exes[3];
  expr _ex;
  if(printer != NULL){
    _ex.type = EXPR;
    exes[0] = symbol_expr2(*printer);
    exes[1] = ex;
    _ex.sub_expr.cnt = 2;
    _ex.sub_expr.exprs = exes;
    ex = _ex;
  }
  expr _exes[20];
  UNUSED(_exes);
  
  compile_status status = COMPILE_OK;
  var_def * evaldef = lispcompile_expr(ex, &status);
  if(COMPILE_ERROR == status || evaldef == NULL) 
    return COMPILE_ERROR;
  
  ASSERT(evaldef != NULL);
  type_def * ret = evaldef->type->fcn.ret;
  size_t ret_size = size_of(ret);

  if(printer != NULL){
    if(ret != &void_def){
      log("Printer should return &void def");
    }
  }else{
    print_def(evaldef->type->fcn.ret); logd(" :: ");  
  }
  
  if(ret == &void_def){
    if(printer == NULL)
      logd("()\n");
    void (* fcn)() = evaldef->data;
    fcn();
    if(printer != NULL) return COMPILE_OK;
  }else if(ret == str2type("(ptr type_def)")){
    type_def * (* fcn)() = evaldef->data;
    fcn();
    logd("type\n");
  }else if(ret == &char_ptr_def){
    char * (* fcn)() = evaldef->data;
    char * str = fcn();
    logd("\"%s\"\n",str);
  }else if(ret == str2type("(ptr symbol)")){
    symbol * (* fcn)() = evaldef->data;
    symbol * s = fcn();
    logd("'%s\n", symbol_name(*s));
    
  }else if(ret->type == POINTER || ret->type == FUNCTION){
    void * (* fcn)() = evaldef->data;
    void * ptr = fcn();
    logd("%p\n", ptr);
  }else if(ret == &f32_def){
    f32 (* fcn)() = evaldef->data;
    f32 v = fcn();
    logd("%f\n",v);  
  }else if(ret == &f64_def){
    f64 (* fcn)() = evaldef->data;
    f64 v = fcn();
    logd("%f\n",v);  
  }else if(ret == &bool_def){
    bool (* fcn)() = evaldef->data;
    bool  v = fcn();
    logd("%s\n", v ? "true" : "false");
  }else if(ret->type == SIMPLE){
    i64 (* fcn)() = evaldef->data;
    i64 v = fcn();
    logd("%i\n",v);
  }else if(ret == &error_def){
    
  }else if(ret == SIMPLE || ret_size  <= 8){

    void * (* fcn)() = evaldef->data;
    void * v = fcn();
    logd("try %p\n", v);
  }else{
    ERROR("Cannot execute function");
  }
  checktypepool();
  return COMPILE_OK;
}

compile_status lisp_run_exprs(expr * exprs, size_t exprcnt){
  for(u32 i = 0; i < exprcnt; i++){
    compile_status s = lisp_run_expr(exprs[i]);    
    if(COMPILE_ERROR == s){
      
      loge("Error at: ");
      print_expr(exprs + i);
      logd("\n");
      return COMPILE_ERROR;
    }
  }
  return COMPILE_OK;
}

compile_status lisp_run_script_file(char * filepath){
  char * code = read_file_to_string(filepath);
  if(code == NULL){
    loge("Error: Could not read code from file '%s'\n", filepath);
    return COMPILE_ERROR;
  }
  compile_status s = lisp_run_script_string(code);
  dealloc(code);
  return s;
}

compile_status lisp_run_script_string(char * code){
  size_t exprcnt;
  expr * exprs = lisp_parse_all(code, &exprcnt);
  compile_status s = lisp_run_exprs(exprs, exprcnt);
  for(size_t i = 0; i < exprcnt; i++)
    delete_expr(exprs + i);
  dealloc(exprs);
  return s;
}
	  
bool test_tcc();
bool test_lisp2c(){
  TEST(test_tcc);
  load_defs();
  compiler_state * c = compiler_make();

  opaque_expr();
  str2type("(fcn (ptr expr) (a (ptr expr)))");
  logd("(fcn (ptr expr) (a (ptr expr)))");

  lisp_load_base();
  bool ret = TEST_SUCCESS;
  push_compiler(c);
  type_def * type = str2type("(fcn void (a (ptr type_def)))");
  type_def * type2 = str2type("(fcn void (a (ptr type_def)))");
  type_def * type3 = str2type("(fcn void (a (ptr void)))");
  ASSERT(type == type2 && type != type3);
  
  type_def * d = str2type("(alias (ptr type_def) td)");
  print_def(d);
  type_def * d2 = str2type("(alias (struct _s1 (x i16) (y i8) (z i16) (x2 i16)) s1)");
  print_def(d2);
  type_def * d3 = str2type("(ptr s1)");
  print_def(d3);	
  type_def * d4 = str2type("(alias (struct _a) a)");
  print_def(d4);

  struct _s1{
    i16 x;
    i8 y;
    i16 z;
    i16 x2;
    // i8 y2;
    // i16 z2;
    // i16 x3;
    // i8 y3;
    // i16 z3;
  };
  logd("sizes: %i %i\n",sizeof(struct _s1), size_of(d2));
  TEST_ASSERT(sizeof(struct _s1) == size_of(d2));
  TEST_ASSERT(sizeof(struct _s1 *) == size_of(d3));
  
  pop_compiler();
  return ret;
}

bool test_tcc(){
  char * code = "extern int * item; int * eval(){ return item;}";
  TCCState * tccs = mktccs();
  int _data2 = 123;
  int * data1 = clone(&_data2,sizeof(int));;
  int fail = tcc_add_symbol(tccs,"item",&data1);
  ASSERT(!fail);
  fail = tcc_compile_string(tccs, code);
  ASSERT(!fail);
  int size = tcc_relocate(tccs, NULL);
  fail = tcc_relocate(tccs, alloc(size));
  ASSERT(!fail);
  int * (* eval)() = tcc_get_symbol(tccs, "eval");
  tcc_delete(tccs);
  TEST_ASSERT(data1 == eval());
  return TEST_SUCCESS;
}
