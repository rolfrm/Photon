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

var_def * get_any_variable(expr * s){
  var_def * stackvar = get_stack_variable(s);
  if(stackvar == NULL)
    stackvar = get_global(s);
  return stackvar;
}

#include <stdbool.h>
#include <ctype.h>
type_def * macrolet_type();
type_def * compile_value(type_def * expected_type, c_block * block, c_value * val, expr * e){
  ASSERT(e->type == VALUE);
  val->type = C_INLINE_VALUE;
  var_def * vdef = NULL;
  if(e->value[0] == '\"')
    {
      CHECK_TYPE(expected_type, char_ptr_def);
      expr * s = symbol_fmt("%.*s",strlen(e->value)-2,e->value + 1);
      expr * bufsym = symbol_fmt("__istr_%i", interned_index(s));
      define_variable(bufsym, type_pool_get(char_ptr_def), s->value, false);
      val->type = C_SYMBOL;
      val->symbol = bufsym;
      return type_pool_get(char_ptr_def);
    }
  bool all_alphanum = true;
  char * number_test = e->value;
  u64 number_len = strlen(e->value);
  if(number_len > 0 && number_test[0] == '-'){
    number_len -= 1;
    number_test += 1;
  }
   
  for(u64 i = 0; i < number_len; i++)
    all_alphanum &= (is_alphanum(number_test[i]) || number_test[i] == '.');
  
  if(all_alphanum){
    bool fits_hex = number_len > 2 && number_test[0] == '0' && ((number_test[1] == 'x' || number_test[1] == 'X') || (number_test[1] == 'b' || number_test[1] == 'B')) ;
    bool fits_alphas = true;
    int dots = 0;
    if(fits_hex){
      for(u64 i = 2; i<number_len; i++)
	fits_hex &= is_hex(number_test[i]);
	
    }else{
      for(u64 i = 0; i<number_len; i++){
	fits_alphas &= isdigit(number_test[i]) || number_test[i] == '.';
	if(e->value[i] == '.')
	  dots++;
	if(dots > 1)
	  fits_alphas = false;
      }
    }
    bool isfloat = dots == 1;
    if(fits_hex || fits_alphas){
      val->raw.value = fmtstr("%s", e->value);
      if(expected_type != NULL){
	if( !isfloat && is_integer_type(expected_type)){
	  val->raw.type = expected_type;
	}else if(is_float_type(expected_type)){
	  val->raw.type = expected_type;
	}else{
	  if(!is_check_type_run()){
	    loge("Unable to convert %s literal '%s' to '", 
		 isfloat? "float" : "integer", e->value);
	    print_decl(expected_type, get_symbol("b")); logd("'.\n");
	  }
	  COMPILE_ERROR("Unable to convert literal.");
	  val->raw.type = isfloat ? &f64_def: &i64_def;
	}
      }else{
	val->raw.type = isfloat ? &f64_def: &i64_def;
      }
	
      return val->raw.type;
      
    }
  }
  val->type = C_SYMBOL;
  val->symbol = e;
  vdef = get_any_variable(e);
  if(vdef == NULL){
    COMPILE_ERROR("Unknown variable '%s'", symbol_name(val->symbol));
  }
  //if(vdef->type == macro_store_type() && vdef->data != NULL){
  //  return expand_macro(expected_type, block, val, &e, 1);
  //}
  if(vdef->type == macrolet_type() && vdef->data != NULL){
    return compile_expr(expected_type, block, val, vdef->data);
  }
  return type_pool_get(vdef->type);
}

expr * expand_macro_store2(void * ms, expr * expr);

type_def * expr2type(expr * typexpr){
  typexpr = intern_expr(typexpr);
  if(typexpr->type == EXPR){
    sub_expr sexp = typexpr->sub_expr;
    COMPILE_ASSERT(sexp.cnt > 0);
    expr * kind = sexp.exprs[0];
    COMPILE_ASSERT(kind->type == VALUE);

    if(strcmp(kind->value, "fcn") == 0){
      type_def out;
      out.type = FUNCTION;
      COMPILE_ASSERT(sexp.cnt > 1);
      type_def * ret = expr2type(sexp.exprs[1]);
      
      COMPILE_ASSERT(error_def != ret);
      type_def * args[sexp.cnt - 2];
      for(size_t i = 0; i < sexp.cnt - 2; i++){
	expr * arg = sexp.exprs[i + 2];
	COMPILE_ASSERT(arg->type == EXPR && arg->sub_expr.cnt == 2 && is_symbol(arg->sub_expr.exprs[0]));
	args[i] = expr2type(arg->sub_expr.exprs[1]);
	COMPILE_ASSERT(args[i] != NULL && args[i] != error_def);
      }
      out.fcn.ret = ret;
      out.fcn.args = args;
      out.fcn.cnt = array_count(args);
      return type_pool_get(&out);

    }else if (strcmp(kind->value,"ptr") == 0){
      COMPILE_ASSERT(sexp.cnt == 2);
      type_def out;
      out.type = POINTER;
      out.ptr.inner = expr2type(sexp.exprs[1]);
      return type_pool_get(&out);
    }else if (strcmp(kind->value, "struct") == 0){
      COMPILE_ASSERT(sexp.cnt >= 2);
      expr * name = intern_expr(sexp.exprs[1]);
      size_t memcnt = sexp.cnt - 2;
      decl members[memcnt];
      for(size_t i = 0 ; i < memcnt; i++){
	expr * sub = sexp.exprs[i + 2];
	COMPILE_ASSERT(sub->type == EXPR && sub->sub_expr.cnt == 2);
	sub_expr sx = sub->sub_expr;
	COMPILE_ASSERT(sx.exprs[0]->type == VALUE);
	members[i].name = intern_expr(sx.exprs[0]);
	members[i].type = expr2type(sx.exprs[1]);
      }
      type_def out;
      out.type = STRUCT;
      out.cstruct.members = clone(members,sizeof(members));
      out.cstruct.cnt = memcnt;
      out.cstruct.name = name;
      return type_pool_get(&out);
    }else if(strcmp(kind->value, "opaque-struct") == 0){
      COMPILE_ASSERT(sexp.cnt == 2);
      type_def out;
      out.type = OPAQUE_STRUCT;
      out.cstruct.members = NULL;
      out.cstruct.cnt = 0;
      out.cstruct.name = intern_expr(sexp.exprs[1]);
      return type_pool_get(&out);
    }else if (strcmp(kind->value, "alias") == 0){
      COMPILE_ASSERT(sexp.cnt == 3);
      type_def out;
      out.type = TYPEDEF;
      out.ctypedef.name = intern_expr(sexp.exprs[2]);
      out.ctypedef.inner = expr2type(sexp.exprs[1]);
      return type_pool_get(&out);
    }
    else{
      
      type_def * td = type_pool_simple(typexpr);
      if(td == NULL){
	// if it is not a simple type, it might be a macro.
	expr * name = intern_expr(sexp.exprs[0]);
	expr e;
	e.type = EXPR;
	e.sub_expr.exprs = sexp.exprs + 1;
	e.sub_expr.cnt = sexp.cnt -1;
	
	var_def * var = get_any_variable(name);
	if(var != NULL && var->type == macro_store_type()){
	  expr * e2 = expand_macro_store2(var->data, &e);
	  return expr2type(e2);
	}
      }else{
	return td;
      }
    }
  }else{
    type_def * td = type_pool_simple(typexpr);
    if(td != error_def)
      return td;
  }
  if(is_check_type_run())
    return error_def;
  logd("Unable to understand type: ");
  print_expr(typexpr);
  ERROR("\n");
  return error_def;
}

type_def * _compile_expr(type_def * expected_type, c_block * block, c_value * value, expr * e){
  sub_expr * se = &e->sub_expr;
  COMPILE_ASSERT(se->cnt > 0);
  expr ** args = se->exprs + 1;
  i64 argcnt = se->cnt - 1;
  {
    expr * name1 = intern_expr(e);
    var_def * var1 = get_any_variable(name1);
    if(var1 != NULL){
      value->type = C_SYMBOL;
      value->symbol = name1;
      if(var1 == NULL){
	COMPILE_ERROR("Unknown variable '%s'", symbol_name(value->symbol));
      }
      return type_pool_get(var1->type);
    }
  }
  expr * name = intern_expr(se->exprs[0]);
  ASSERT(name != NULL);
  void * var_data;
  type_def * var_type;
  {
    var_def * fvar = get_any_variable(name);
    if(fvar == NULL) COMPILE_ERROR("unknown symbol '%s'", symbol_name(name));
    ASSERT(fvar->name == name);
    var_data = fvar->data;
    var_type = fvar->type;
  }
  
  if(var_type == type_pool_get(&cmacro_def_def)){
    cmacro_def * macro = var_data;

    if(macro->arg_cnt != argcnt && macro->arg_cnt != -1){
      if(!is_check_type_run()){
	print_expr(name); logd(" : ");
      }
      COMPILE_ERROR("Unsupported number of arguments %i",argcnt); 
      COMPILE_ERROR("");
    }
    type_def * ( *macro_fcn)(type_def * ex_type, c_block * block, c_value * val, ...) = macro->fcn;
    type_def * td = NULL;
    switch(macro->arg_cnt){
    case 0:
      td =  macro_fcn(expected_type, block, value);
      break;
    case 1:
      td =  macro_fcn(expected_type, block, value, args[0]);
      break;
    case 2:
      td =  macro_fcn(expected_type, block, value, args[0], args[1]);
      break;
    case 3:
      td =  macro_fcn(expected_type, block, value, args[0], args[1], args[2]);
      break;
    case 4:
      td =  macro_fcn(expected_type, block, value, args[0], args[1], args[2],
		      args[3]);
      break;
    case 5:
      td =  macro_fcn(expected_type, block, value, args[0], args[1], args[2],
		      args[3], args[4]);
      break;
    case 6:
      td =  macro_fcn(expected_type, block, value, args[0], args[1], args[2],
		      args[3], args[4], args[5]);
      break;
    case -1:
      td = ((type_def *(*)(type_def * exp, c_block * block, c_value * value, expr **,size_t))macro->fcn)
	(expected_type, block,value,args, argcnt);
      break;
    default:
      ERROR("Number of macro arguments not supported: %i", argcnt);
    }
    if(td == error_def && !is_check_type_run()){
      loge("Unable to compile: ");
      expr e = {.type = EXPR};
      e.sub_expr = *se;
      print_expr(&e); loge("\n");
    }

    return td;

  }else if(var_type->type == FUNCTION ||
	   (var_type->type == POINTER && var_type->ptr.inner->type == FUNCTION)){
    type_def * td = var_type->type == POINTER ? var_type->ptr.inner : var_type;

    if(td->fcn.cnt != argcnt){
      loge("Too few arguments to function ");
      print_expr(name);
      COMPILE_ERROR("got %i expected %i.", argcnt, td->fcn.cnt);
    }
    c_value fargs[argcnt];
    memset(fargs,0,sizeof(c_value) * argcnt);
    type_def * farg_types[argcnt];
    int err_arg = -1;
    for(i64 i = 0; i < argcnt; i++){
      farg_types[i] = compile_expr(td->fcn.args[i], block, fargs + i, args[i]);
      if(td->fcn.args[i] != farg_types[i]){
	char buf[10];
	sprintf(buf, "arg%i", i);
	expr * fcnsym = get_symbol(buf);
	err_arg = i;
	loge("Error at argument %i of call to ", i); print_expr(name);loge("\n");
	logd("\nExpected: ");
	print_decl(td->fcn.args[i], fcnsym);
	logd("\nGot     : ");
	print_decl(farg_types[i], fcnsym);
	logd("\n\n");
	break;
      }
    }

    if(err_arg >= 0)
      COMPILE_ERROR("Non matching types for function 's' arg %i\n", err_arg);

    c_function_call call;
    call.type = td;
    call.name = name;
    call.args = clone(fargs,sizeof(fargs));
    call.arg_cnt = argcnt;

    value->type = C_FUNCTION_CALL;
    value->call = call;
    return td->fcn.ret;
  }else if(var_type == macro_store_type()){
    return expand_macro(expected_type, block, value, se->exprs, se->cnt);
  }
  /*else if(var_type == macrolet_type() && var_data != NULL){
    return compile_expr(expected_type, block, value, var_data);
    }*/
  else{
    print_decl(var_type, get_symbol("t"));logd("\n");
    COMPILE_ERROR("Not supported\n");
  }
  return error_def;
}

type_def * compile_expr(type_def * expected_type, c_block * block, c_value * val,  expr * e ){

  switch(e->type){
  case EXPR:
    return _compile_expr(expected_type, block, val, e);
    break;
  case VALUE:
    return compile_value(expected_type, block,  val,e);
    break;
  case ERROR:
    return error_def;
  }
  return error_def;
}

c_root_code compile_lisp_to_eval(expr * exp, compile_status * status){
  static int eval_id = 0;
  eval_id++;
  c_root_code r;
  r.type = C_FUNCTION_DEF;
  r.fcndef.block = c_block_empty;
  char name[100];
  sprintf(name, "__eval%i", eval_id);
  r.fcndef.name = get_symbol(name);

  c_expr expr = {.type = C_VALUE};
  type_def * t = compile_expr(NULL, &r.fcndef.block, &expr.value, exp);
  if(t == error_def){
    *status = COMPILE_ERROR;
    eval_id--;
    return r;
  }
  type_def td = {.type = FUNCTION, .fcn = {.ret = t, .args = NULL, .cnt = 0}};
  r.fcndef.type = type_pool_get(&td);

  if(t != type_pool_get(&void_def)) expr.type = C_RETURN;
  block_add(&r.fcndef.block, expr);
  *status = COMPILE_OK;
  eval_id--;
  return r;
}

type_def * str2type(char * str){
  expr e;
  if(lisp_parse(str, &e) == NULL)
    return error_def;
  type_def * td = expr2type(&e);
  delete_expr(&e);
  return td;
}

#include <libtcc.h>
#include <stdlib.h>

char *getcwd(char *buf, size_t size);
i32 chdir(char * path);

char * orig_dir = NULL;
void load_orig_dir(char * photon_path){
  static char dirbuf2[100];
  char dirbuf[100];
  // Use OS to figure out orig directory.
  getcwd(dirbuf,array_count(dirbuf));
  enter_dir_of(photon_path);
  getcwd(dirbuf2,array_count(dirbuf2));
  chdir(dirbuf);
  orig_dir = dirbuf2;
}

char * get_orig_dir(){
  ASSERT(orig_dir != NULL);
  return orig_dir;
}

char * _tcc_error = NULL;

void tccerror(void * opaque, const char * msg){
  UNUSED(opaque);
  _tcc_error = clone((char *)msg, strlen(msg));
}

TCCState * mktccs(){
  TCCState * tccs = tcc_new();
  char * dir = get_orig_dir();
  tcc_set_lib_path(tccs, dir);
  tcc_set_error_func(tccs, NULL, tccerror);
  tcc_set_output_type(tccs, TCC_OUTPUT_MEMORY);
  //tcc_set_options(tccs, "-g");
  return tccs;
}


void go_write(type_def ** deps, expr ** vdeps, c_root_code * codes, size_t code_cnt){

  write_dependencies(deps);
  for(size_t i = 0; vdeps[i] != NULL; i++){
    var_def * var = get_global(vdeps[i]);
    if(var == NULL){
      ERROR("No global variable named 's'");//, symbol_name(vdeps[i]));
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

void checkvdeps(expr ** vdep){
  int it = 0;
  while(vdep[it] != NULL){
    if(vdep[it] == NULL){
      ERROR("Symbol NULL %i", it);
    }
    //if(type_pool_simple(*vdep) != NULL){
    //  logd("Name '");print_expr(vdep[it]);logd("' already used as type.\n");
    //  ERROR("This usecase currently not supported.\n");
    //}
    it++;
  }
}

void * compile_as_c(c_root_code * codes, size_t code_cnt){
  type_def * deps[1000];
  expr * vdeps[1000];
  memset(deps, 0, sizeof(deps));
  memset(vdeps, 0, sizeof(vdeps));
  for(size_t i = 0; i < code_cnt; i++)
    c_root_code_dep(deps, vdeps, codes[i]);

  checkvdeps(vdeps);
  char buf[100];
  static int tmp_idx = 0;
  sprintf(buf, "__tmp_file%i", tmp_idx++);
  FILE * f = fopen(buf,"wb+");
  push_format_out(f);
  go_write(deps, vdeps, codes, code_cnt);
  pop_format_out();

  char * data = read_stream_to_string(f);
  size_t datasize = ftell(f);

  ASSERT(data != NULL);
  fclose(f);
  remove((const char *) buf);
  char header[] = "//***********\n";
  char * compile_out_path = get_compile_out(lisp_current_compiler);
  if(compile_out_path != NULL){
    append_buffer_to_file(header,sizeof(header) - 1, compile_out_path);
    append_buffer_to_file(data, datasize,compile_out_path);
  }
  TCCState * tccs = mktccs();
  for(size_t i = 0; i < array_count(vdeps) && vdeps[i] != NULL; i++){
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
  if(_tcc_error != NULL){
    fail = true;
    loge("TCC Error: '%s'\n", _tcc_error);
    dealloc(_tcc_error);
    _tcc_error = NULL;
  }
  free(data);
  ASSERT(!fail);
  if(fail) return NULL;
  data = NULL;
  int size = tcc_relocate(tccs, NULL);
  void * code_buffer = alloc(size);
  fail = tcc_relocate(tccs, code_buffer);
  ASSERT(!fail);
  if(fail) {
    dealloc(code_buffer);
    return NULL;
  }
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

void lisp_load_base(char * photon_directory){
  // Make sure the initial path is loaded.
  load_orig_dir(photon_directory);

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
var_def * lisp_compile_expr(expr * ex, compile_status * optout_status){
  expr * name = NULL;
  c_root_code cl = compile_lisp_to_eval(ex, optout_status);
  if(*optout_status == COMPILE_ERROR)
    return NULL;
  if(cl.fcndef.type->fcn.ret == error_def){
    if(optout_status != NULL)
      *optout_status = COMPILE_ERROR;
    return NULL;
  }
  
  void * codebuf = compile_as_c(&cl,1);
  if(codebuf == NULL)
    *optout_status = COMPILE_ERROR;
  
  name = cl.fcndef.name;
  c_root_code_delete(cl);
  
  
  if(codebuf != NULL)
    add_delete_soon(codebuf);

  if(*optout_status == COMPILE_ERROR)
    return NULL;
  return get_global(name);
}

expr * printer = NULL;

compile_status lisp_run_expr(expr * _ex){
  static int run_level = 0;
  run_level++;
  expr * exes[3];
  expr ex = *_ex;

  if(printer != NULL){
    ex.type = EXPR;
    exes[0] = printer;
    exes[1] = _ex;
    ex.sub_expr.cnt = 2;
    ex.sub_expr.exprs = exes;
  }
  compile_status status = COMPILE_OK;
  var_def * evaldef = lisp_compile_expr(intern_expr(&ex), &status);

  if(COMPILE_ERROR == status || evaldef == NULL)
    return COMPILE_ERROR;

  ASSERT(evaldef != NULL);
  type_def * ret = evaldef->type->fcn.ret;
  size_t ret_size = size_of(ret);

  if(printer != NULL){

    if(ret != &void_def)
      log("Printer should return &void def");
  }else{
    print_def(evaldef->type->fcn.ret);
    logd(" :: ");
  }

  if(ret == &void_def){
    if(printer == NULL)
      logd("()\n");

    void (* fcn)() = evaldef->data;
    fcn();
  }else if(ret == str2type("(ptr type_def)")){
    type_def * (* fcn)() = evaldef->data;
    fcn();
    logd("type\n");
  }else if(ret == char_ptr_def){
    char * (* fcn)() = evaldef->data;
    char * str = fcn();
    logd("\"%s\"\n",str);
  }else if(ret == str2type("(ptr symbol)")){
    symbol * (* fcn)() = evaldef->data;
    symbol * s = fcn();
    UNUSED(s);

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
  }else if(ret == error_def){
    ERROR("Error\n");
  }else if(ret == SIMPLE || ret_size  <= 8){
    void * (* fcn)() = evaldef->data;
    void * v = fcn();
    logd("try %p\n", v);
  }else{
    ERROR("Cannot execute function");
  }
  checktypepool();
  run_level--;
  if(run_level == 0)
    run_delete();
  return COMPILE_OK;
}

compile_status lisp_run_exprs(expr * exprs, size_t exprcnt){
  for(u32 i = 0; i < exprcnt; i++){
    expr * e = exprs + i;
    compile_status s = lisp_run_expr(e);
    if(COMPILE_ERROR == s){

      loge("Error at: ");
      print_expr(e);
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
  expr _expr;
  compile_status s;
  while(NULL != (code = lisp_parse(code, &_expr))){
     s = lisp_run_exprs(&_expr, 1);
     if(s == COMPILE_ERROR)
       break;
     delete_expr(&_expr);
  }
  return s;
}

bool test_tcc();
bool test_lisp2c(){
  TEST(test_tcc);
  load_defs();
  lisp_current_compiler = lisp_make_compiler();

  opaque_expr();
  str2type("(fcn (ptr expr) (a (ptr expr)))");
  logd("(fcn (ptr expr) (a (ptr expr)))");
  
  lisp_load_base(".");
  bool ret = TEST_SUCCESS;
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
