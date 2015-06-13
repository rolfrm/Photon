#include <stdio.h>
#include <iron/full.h>
#include "lisp_parser.h"
#include <libtcc.h>
#include "lisp_types.h"
#include "lisp_std_types.h"
#include "lisp_compiler.h"
#include "type_pool.h"
#include "expr_utils.h"
#include "builtin_macros.h"
	
typedef struct{
  c_root_code * c_code;
  size_t code_cnt;
}compiled_lisp;

type_def * compile_value(c_value * val, value_expr e){
  val->type = C_INLINE_VALUE;
  var_def * vdef;
  switch(e.type){
  case STRING:
    val->raw.value = fmtstr("\"%.*s\"",e.strln,e.value);
    val->raw.type = &char_ptr_def;
    return type_pool_get(&char_ptr_def);
  case KEYWORD:
  case SYMBOL:
    val->type = C_SYMBOL;
    val->symbol = vexpr_symbol(e);
    vdef = get_variable(val->symbol);
    if(vdef == NULL){
      COMPILE_ERROR("Unknown variable '%s'", symbol_name(val->symbol));
    }
    return type_pool_get(vdef->type);
  case NUMBER:
    val->raw.value = fmtstr("%.*s",e.strln,e.value);
    val->raw.type = &i64_def;
    return type_pool_get(&i64_def);
    break;
  default:
    break;
  }
  logd("type: %i\n", e.type);
  COMPILE_ERROR(false);
}

type_def * _type_macro(expr typexpr){
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
      type_def * ret = _type_macro(sexp.exprs[1]);
    
      COMPILE_ASSERT(&error_def != ret);
      type_def * args[sexp.cnt - 2];
      for(size_t i = 0; i < sexp.cnt - 2; i++){
	expr arg = sexp.exprs[i + 2];
	COMPILE_ASSERT(arg.type == EXPR && arg.sub_expr.cnt == 2 && is_symbol(arg.sub_expr.exprs[0]));
	args[i] = _type_macro(arg.sub_expr.exprs[1]);
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
      out.ptr.inner = _type_macro(sexp.exprs[1]);
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
	members[i].type = _type_macro(sx.exprs[1]);
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
      out.ctypedef.inner = _type_macro(sexp.exprs[1]);
      return type_pool_get(&out);
    }
  }else{
    type_def * td = type_pool_simple(expr_symbol(typexpr));
    td = type_pool_get(td);
    COMPILE_ASSERT(td != NULL);
    return td;
  }
  return &error_def;
}

type_def * __compile_expr(c_block * block, c_value * value, sub_expr * se){
  UNUSED(value);
  if(se->cnt == 0)
    ERROR("sub expressio count 0");
  expr name_expr = se->exprs[0];
  if(name_expr.type != VALUE && name_expr.value.type != SYMBOL) ERROR("need symbol for first car");
  
  expr * args = se->exprs + 1;
  i64 argcnt = se->cnt - 1;

  symbol name = expr_symbol(name_expr);

  var_def * fvar = get_variable(name);
  if(fvar == NULL) COMPILE_ERROR("unknown symbol '%s'", symbol_name(name));
  if(fvar->type == type_pool_get(&cmacro_def_def)){
    cmacro_def * macro = fvar->data;

    if(macro->arg_cnt != argcnt && macro->arg_cnt != -1)
      ERROR("Unsupported number of arguments %i for %s",argcnt, macro->name);

    switch(macro->arg_cnt){
    case 0:
      return ((type_def *(*)(c_block * block, c_value * value)) macro->fcn)(block,value);
    case 1:
      return ((type_def *(*)(c_block * block, c_value * value,expr)) macro->fcn)(block,value,args[0]);
    case 2:
      return ((type_def *(*)(c_block * block, c_value * value,expr,expr)) 
	      macro->fcn)(block,value,args[0],args[1]);
    case 3:
      return ((type_def *(*)(c_block * block, c_value * value, expr,expr,expr)) 
	      macro->fcn)(block,value,args[0],args[1],args[2]);
    case -1:

      return ((type_def *(*)(c_block * block, c_value * value, expr *,size_t))macro->fcn)(block,value,args, argcnt);
    default:
      ERROR("Number of macro arguments not supported: %i", argcnt);
    }
  }else if(fvar->type->type == FUNCTION){
    type_def * td = fvar->type;
    COMPILE_ASSERT(td->fcn.cnt == argcnt);

    c_value fargs[argcnt];
    type_def * farg_types[argcnt];
    for(i64 i = 0; i < argcnt; i++){
      farg_types[i] = _compile_expr(block, fargs + i, args[i]);
      if(type_pool_get(farg_types[i]) != type_pool_get(td->fcn.args[i])){
	logd("ERROR: got '");
	print_min_type(farg_types[i]);
	logd("' expected '");
	print_min_type(td->fcn.args[i]);
	logd("'\n");
	ERROR("Non matching types");
      }
    }
    
    c_function_call call;
    call.type = td;
    call.name = fvar->name;
    
    call.args = clone(fargs,sizeof(fargs));
    value->type = C_FUNCTION_CALL;
    value->call = call;
    value->call.arg_cnt = argcnt;
    
    return td->fcn.ret;
  }else{
    ERROR("Not supported.. %i\n", fvar->type->type);
  }
  type_def * sub_types[se->cnt];
  UNUSED(sub_types);
  c_value val[se->cnt];
  for(size_t i = 0; i < se->cnt; i++){
    expr * e = se->exprs + i;
    sub_types[i] = _compile_expr(block, val + i, *e);
  }
  return &error_def;
}
	  
type_def * _compile_expr(c_block * block, c_value * val,  expr e ){
  type_def * td;
  switch(e.type){
  case EXPR:
    return __compile_expr(block, val, &e.sub_expr);
    break;
  case VALUE:
    td = compile_value(val,e.value);
    break;
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
  type_def * t = _compile_expr(&f->block, &val, exp);
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
  return _type_macro(lisp_parse1(str));
}

void print_type(type_def * def){
  logd("type: '")print_def(def); logd("'\n");
}
void write_line(char * str){
  logd("%s\n", str);
}

#include <libtcc.h>
#include <stdlib.h>
void tccerror(void * opaque, const char * msg){
  UNUSED(opaque);
  format("%s\n",msg);
}

TCCState * mktccs(){
  TCCState * tccs = tcc_new();
  tcc_set_lib_path(tccs,".");
  tcc_set_error_func(tccs, NULL, tccerror);
  tcc_set_output_type(tccs, TCC_OUTPUT_MEMORY);
  return tccs;
}

void go_write(type_def ** deps, symbol * vdeps, c_root_code * codes, size_t code_cnt){

  write_dependencies(deps);
  for(size_t i = 0; deps[i] != NULL; i++){
    if(deps[i]->type == TYPEDEF){
      continue;
      print_def(deps[i]->ctypedef.inner);
    }else{
      print_def(deps[i]);
    }
    format(";\n");
  }
    
  for(size_t i = 0; vdeps[i].id != 0; i++){
      
    var_def * var = get_variable(vdeps[i]);
    ASSERT(var != NULL);
    decl dcl;
    dcl.name = var->name;
    dcl.type = var->type;
    format("extern ");
    print_cdecl(dcl);format(";\n");
  }
  
  for(size_t i = 0; i < code_cnt; i++){
    c_root_code_dep(deps, vdeps, codes[i]);
    print_c_code(codes[i]);
  }
}

void compile_as_c(c_root_code * codes, size_t code_cnt){
  type_def * deps[100];
  symbol vdeps[100];
  memset(deps, 0, sizeof(deps));
  memset(vdeps, 0, sizeof(vdeps));
  for(size_t i = 0; i < code_cnt; i++){
    c_root_code_dep(deps, vdeps, codes[i]);
  }
  char * data = NULL;
 size_t cnt = 0;
  FILE * f = open_memstream(&data, &cnt);
  push_format_out(f);
  go_write(deps, vdeps, codes, code_cnt);
  pop_format_out();
  fclose(f);

  char header[] = "//***********\n";
  append_buffer_to_file(header,sizeof(header) - 1,"compile_out.c");
  append_buffer_to_file(data,cnt,"compile_out.c");
  TCCState * tccs = mktccs();
  for(size_t i = 0; i < array_count(vdeps) && vdeps[i].id != 0; i++){

    var_def * var = get_variable(vdeps[i]);
    if(var->type->type == FUNCTION){
      int fail = tcc_add_symbol(tccs,get_c_name(var->name),var->data);
      ASSERT(!fail);
    }else{
      char * vname = get_c_name(var->name);
      int fail = tcc_add_symbol(tccs,vname,var->data);
      ASSERT(!fail);
    }
  }
  
  int fail = tcc_compile_string(tccs, data);
  free(data);
  ASSERT(!fail);
  data = NULL;
  int size = tcc_relocate(tccs, NULL);
  fail = tcc_relocate(tccs, alloc(size));
  ASSERT(!fail);
  
  for(size_t i = 0; i < code_cnt; i++){
    c_root_code r = codes[i];
    if(r.type == C_FUNCTION_DEF){
      void * ptr = tcc_get_symbol(tccs, get_c_name(r.fcndef.name));
      ASSERT(ptr != NULL);
      compiler_define_variable_ptr(r.fcndef.name, r.fcndef.type, ptr);
    }else if(r.type == C_VAR_DEF){

      decl vdecl = r.var.var;
      void * ptr = tcc_get_symbol(tccs, get_c_name(vdecl.name));
      ASSERT(ptr != NULL);
      compiler_define_variable_ptr(vdecl.name, vdecl.type, ptr);
    }
  }

  tcc_delete(tccs);
}

i64 i64_add(i64 a, i64 b){
  return a + b;
}

double double_add(double a, double b){ return a + b;}
double double_sub(double a, double b){ return a - b;}
double double_div(double a, double b){ return a / b;}
double double_mul(double a, double b){ return a * b;}
void defun(char * name, type_def * t, void * fcn){
  compiler_define_variable_ptr(get_symbol(name), t, fcn);
}

void lisp_load_compiler(compiler_state * c){
  push_compiler(c);
	
  // Types
  load_defs();
  
  builtin_macros_load();

  // Functions
  type_def * type = str2type("(fcn void (a (ptr type_def)))");
  compiler_define_variable_ptr(get_symbol("print_type"), type, print_type);
  compiler_define_variable_ptr(get_symbol("write_line"), 
			       str2type("(fcn void (a (ptr char)))"), &write_line);
  compiler_define_variable_ptr(get_symbol("i64+"), 
			       str2type("(fcn i64 (a i64) (b i64))"), &i64_add);
  
  compiler_define_variable_ptr(get_symbol("get-symbol"), 
			       str2type("(fcn (ptr symbol) (a (ptr char)))"), get_symbol2);
  
  type_def * d2t =  str2type("(fcn f64 (a f64) (b f64))");
  defun("f+", d2t, double_add);
  defun("f-", d2t, double_sub);
  defun("f/", d2t, double_div);
  defun("f*", d2t, double_mul);
  pop_compiler();
}

var_def * lisp_compile_expr(expr ex){
  c_root_code cl = compile_lisp_to_eval(ex);
  if(cl.fcndef.type->fcn.ret == &error_def)
    return NULL;
  compile_as_c(&cl,1);
  symbol s = get_symbol("eval");
  return get_variable(s);
}

void * lisp_compile_and_run_expr(expr ex){
  var_def * var = lisp_compile_expr(ex);
  void * (*fcn)() = var->data;
  ASSERT(fcn != NULL);
  return fcn();
}


void lisp_run_expr(expr ex){

  var_def * evaldef = lisp_compile_expr(ex);
  ASSERT(evaldef != NULL);
  print_def(evaldef->type->fcn.ret); logd(" :: ");
  type_def * ret = evaldef->type->fcn.ret;
  if(ret == &void_def){
    logd("()\n");
    void (* fcn)() = evaldef->data;
    fcn();
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
  }else if(ret->type == SIMPLE){
    i64 (* fcn)() = evaldef->data;
    i64 v = fcn();
    logd("%i\n",v);
  }else if(ret == &error_def){
    
  }else{
    void * (* fcn)() = evaldef->data;
    void * v = fcn();
    logd("try %p\n", v);
  }
}

void lisp_run_exprs(compiler_state * c, expr * exprs, size_t exprcnt){
  lisp_load_compiler(c);
  with_compiler(c,lambda(void, (){
	for(size_t i = 0; i < exprcnt; i++){
	  lisp_run_expr(exprs[i]);
	}};));
}



void lisp_run_script_file(char * filepath){
  char * code = read_file_to_string(filepath);
  lisp_run_script_string(code);
  dealloc(code);
}

void lisp_run_script_string(char * code){
  size_t exprcnt;
  expr * exprs = lisp_parse_all(code, &exprcnt);
  lisp_run_exprs(get_compiler(), exprs, exprcnt);
}
bool test_tcc();
bool test_lisp2c(){
  TEST(test_tcc);
  load_defs();
  compiler_state * c = compiler_make();

  opaque_expr();
  str2type("(fcn (ptr expr) (a (ptr expr)))");
  logd("(fcn (ptr expr) (a (ptr expr)))");

  lisp_load_compiler(c);
  bool ret = TEST_SUCCESS;
  push_compiler(c);
  type_def * type = str2type("(fcn void (a (ptr type_def)))");
  type_def * type2 = str2type("(fcn void (a (ptr type_def)))");
  type_def * type3 = str2type("(fcn void (a (ptr void)))");
  ASSERT(type == type2 && type != type3);
  
  type_def * d = str2type("(alias (ptr type_def) td)");
  print_def(d);
  type_def * d2 = str2type("(alias (struct _vec2 (x f32) (y f32)) vec2)");
  print_def(d2);
  type_def * d3 = str2type("(ptr vec2)");
  print_def(d3);	
  type_def * d4 = str2type("(alias (struct _a) a)");
  print_def(d4);
  

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
