#include <iron/full.h>
#include "lisp_parser.h"
#include "lisp_types.h"
#include "c_ast.h"
#include "lisp_compiler.h"
#include "expr_utils.h"
#include <dlfcn.h>
#include "lisp_std_types.h"
void defun(char * name, type_def * t, void * fcn){
  define_variable(get_symbol(name), t, fcn, true);
}

void print_type(type_def * def){
  print_decl(def, get_symbol("t"));
}

type_def * ptr_inner(type_def * ptr_def){
  
  if(ptr_def == error_def || ptr_def->type != POINTER){
    return error_def;
  }
  return ptr_def->ptr.inner;
}

expr _type2expr(type_def * type_def){
  if(type_def->type == POINTER){
    expr subs[2];
    subs[0] = symbol_expr("ptr");
    subs[1] = _type2expr(type_def->ptr.inner);
    expr o;
    o.type = EXPR;
    o.sub_expr.exprs = clone(subs,sizeof(subs));
    o.sub_expr.cnt = 2;
    return o;
  }
  if(type_def->type == SIMPLE){
    return symbol_expr2(type_def->simple.name);
  }
  if(type_def->type == TYPEDEF){
    return symbol_expr2(type_def->ctypedef.name);
  }
  ERROR("Not supported");
  expr e;
  return e;
}

expr * type2expr(type_def * ptr_def){
  expr e = _type2expr(ptr_def);
  return clone(&e, sizeof(e));
}

void * load_lib(char * path){
  void * handle =  dlopen(path, RTLD_LAZY);
  if(handle == NULL)
    loge("Unable to load library '%s'\n", path);
  return handle;
}

void * load_symbol(void * lib, symbol * sym, symbol * name, type_def * t){
  void * ptr = dlsym(lib, get_c_name(*name));
  if(ptr == NULL) {
    loge("Error no such symbol '%s'\n", symbol_name(*name));}
  else {
    define_variable(*sym, t, ptr, true);
  }
  return ptr;
}

// 3 different type_of's. one disables error handling, needed when doing type_of operations.

type_def * type_of3(type_def * expected_type, expr * ex){
  c_block blk = c_block_empty;
  c_value val = {.type = C_NOTHING};
  type_def * td = compile_expr(expected_type, &blk, &val, *ex);
  c_block_delete(blk);
  c_value_delete(val);
  return td;
}

type_def * type_of2(type_def * expected_type, expr * ex){

  bool prev_print = lisp_print_errors;
  lisp_print_errors = false;
  type_def * td = type_of3(expected_type, ex);
  lisp_print_errors = prev_print;
  return td;
}

type_def * type_of(expr * ex){
  return type_of2(NULL, ex);
}

char * symbol_name2(symbol * sym){
  return symbol_name(*sym);
}

bool is_fcn_type(type_def * t){
  return t->type == FUNCTION;
}

bool is_ptr_type(type_def * t){
  return t != NULL && t->type == POINTER;
}

type_def ** fcn_arg_types(type_def * td){
  if(td->type != FUNCTION) return NULL;
  return td->fcn.args;
}

u64 fcn_arg_cnt(type_def * td){
  ASSERT(td->type == FUNCTION);
  return td->fcn.cnt;
}

type_def * fcn_ret_type(type_def * td){
  ASSERT(td->type == FUNCTION);
  return td->fcn.ret;
}

extern symbol * printer;
void set_printer(symbol * sym){
  printer = sym;  
}

type_def * var_type(symbol * sym){
  var_def * var = get_any_variable(*sym);
  if(var != NULL)
    return var->type;
  return NULL;
}

void * get_var(symbol * sym){
  var_def * var = get_global(*sym);
  if(var == NULL) return NULL;
  if(var->is_ptr)
    return var->data;
  return var->data;
}

#include <iron/coroutines.h>

void builtin_print_string(char * str){
  logd("%s\n", str);
}

bool is_check_type_run(){
  return !lisp_print_errors;
}

bool start_read_eval_print_loop();

void load_functions(){
  defun("print-type", str2type("(fcn void (a (ptr type_def)))"), print_type);
  defun("builtin-print-str", str2type("(fcn void (str (ptr char)))"), builtin_print_string);
  defun("get-symbol", str2type("(fcn (ptr symbol) (a (ptr char)))"), get_symbol2);
  defun("size-of",str2type("(fcn u64 (type (ptr type_def)))"), size_of);
  str2type("(alias (ptr (opaque-struct _lib)) lib)"); // declare the lib tyoedef
  defun("load-lib",str2type("(fcn lib (libname (ptr char)))"), load_lib);
  type_def * loadsymbol = str2type("(fcn (ptr void) (_lib lib) (sym (ptr symbol)) (name (ptr symbol)) (t (ptr type_def)))");
  defun("load-symbol", loadsymbol, load_symbol);
  defun("type-of",str2type("(fcn (ptr type_def) (expr (ptr expr)))"), type_of);
  defun("type-of2",str2type("(fcn (ptr type_def) (expected_type (ptr type_def)) (expr (ptr expr)))"), 
	type_of2);
  defun("type-of3",str2type("(fcn (ptr type_def) (expected_type (ptr type_def)) (expr (ptr expr)))"), 
	type_of3);
  defun("check-type-run?", str2type("(fcn bool)"), is_check_type_run);
  defun("print-expr", str2type("(fcn void (theexpr (ptr expr)))"), print_expr);
  defun("ptr-inner", str2type("(fcn (ptr type_def) (ptr (ptr type_def)))"),  ptr_inner);
  defun("type2expr", str2type("(fcn (ptr expr) (t (ptr type_def)))"), type2expr);
  defun("symbol-name", str2type("(fcn (ptr char) (sym (ptr symbol)))"), symbol_name2);
  defun("var-type", str2type("(fcn (ptr type_def) (variable (ptr symbol)))"), var_type);
  defun("get-var", str2type("(fcn (ptr void) (sym (ptr symbol)))"), get_var);
  defun("is-fcn-type?", str2type("(fcn bool (type (ptr type_def)))"), is_fcn_type);
  defun("is-ptr-type?", str2type("(fcn bool (type (ptr type_def)))"), is_ptr_type);
  defun("is-integer-type?", str2type("(fcn bool (type (ptr type_def)))"), is_integer_type);
  defun("is-float-type?", str2type("(fcn bool (type (ptr type_def)))"), is_float_type);

  defun("fcn-arg-types", str2type("(fcn (ptr (ptr type_def)) (t (ptr type_def)))"), fcn_arg_types);
  defun("fcn-ret-type", str2type("(fcn (ptr type_def) (t (ptr type_def)))"), fcn_ret_type);
  defun("fcn-arg-cnt", str2type("(fcn u64 (t (ptr type_def)))"), fcn_arg_cnt);
  defun("set-printer", str2type("(fcn void (ptr (ptr symbol)))"), set_printer);

  str2type("(alias (opaque-struct _ccdispatch) ccdispatch)");

  /*
  defun("ccstart", str2type("(fcn (ptr ccdispatch))"), ccstart);
  defun("ccyield", str2type("(fcn void)"), ccyield);
  defun("ccstep", str2type("(fcn void (cc (ptr ccdispatch)))"), ccstep);
  defun("ccthread", str2type("(fcn void (cc (ptr ccdispatch)) (fcn (fcn void (arg (ptr void)))) (userdata (ptr void)))"), ccthread);
  defun("repl", str2type("(fcn bool)"), start_read_eval_print_loop);
  */
  defun("timestamp", str2type("(fcn i64)"), timestamp);
}
