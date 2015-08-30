#include <iron/full.h>
#include "lisp_parser.h"
#include "lisp_types.h"
#include "c_ast.h"
#include "lisp_compiler.h"
#include "expr_utils.h"

#include "lisp_std_types.h"

void defun2(char * name, type_def * t, void * fcn){

  define_variable(get_symbol(name), t, fcn, true);
}

void defun(char * name, char * ttype, void * fcn){

  defun2(name, str2type(ttype), fcn);
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

bool is_linux(){
  #ifdef _WIN32
  return false;
  #else
  return true;
  #endif
}

#ifdef _WIN32

#undef ASSERT
#include <windows.h>
void * load_lib(char * path){
  void * handle =  LoadLibrary(path);
  if(handle == NULL)
    loge("Unable to load library '%s'\n", path);
  return handle;
}

void * load_symbol(void * lib, symbol * name){
  return GetProcAddress((HMODULE)lib, get_c_name(*name));
}
#define ASSERT(x)
#else
#include <dlfcn.h>
void * load_lib(char * path){
  void * handle =  dlopen(path, RTLD_LAZY);
  if(handle == NULL)
    loge("Unable to load library '%s'\n", path);
  return handle;
}

void * load_symbol(void * lib, char * name){
  return dlsym(lib, name);
}

#endif
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

void eval(expr * e){
  lisp_run_expr(*e);
}

void builtin_print_string(char * str){
  logd("%s\n", str);
}

bool is_check_type_run(){
  return !lisp_print_errors;
}

char * current_dir = ".";
char *getcwd(char *buf, size_t size);
void chdir(char * path);

void load_builtin(char * path){

  char dirbuf[100];
  getcwd(dirbuf,array_count(dirbuf));
  enter_dir_of(path);
  char filename[30];
  get_filename(filename, path);
  compile_status s = lisp_run_script_file(filename);
  if(s == COMPILE_ERROR){
    loge("Unable to compile and run '%s'.\n", path);
  }
  chdir(dirbuf);
}

bool start_read_eval_print_loop();


void load_functions(){
  defun("print-type", ("(fcn void (a (ptr type_def)))"), print_type);
  defun("eval!", ("(fcn void (expr (ptr expr)))"), eval);
  defun("load", ("(fcn void (file (ptr char)))"), load_builtin);
  defun("builtin-print-str", ("(fcn void (str (ptr char)))"), builtin_print_string);
  defun("get-symbol", ("(fcn (ptr symbol) (a (ptr char)))"), get_symbol2);
  defun("size-of",("(fcn u64 (type (ptr type_def)))"), size_of);
  defun("defun!", ("(fcn void (name (ptr char)) (t (ptr type_def)) (p (ptr void)))"), defun2);
  str2type("(alias (ptr (opaque-struct _lib)) lib)"); // declare the lib tyoedef
  defun("is-linux?",("(fcn bool)"), &is_linux);
  defun("load-lib",("(fcn lib (libname (ptr char)))"), load_lib);
  defun("load-symbol", ("(fcn (ptr void) (_lib lib) (name (ptr char)))"), load_symbol);
  defun("type-of",("(fcn (ptr type_def) (expr (ptr expr)))"), type_of);
  defun("type-of2",("(fcn (ptr type_def) (expected_type (ptr type_def)) (expr (ptr expr)))"),
	type_of2);
  defun("type-of3",("(fcn (ptr type_def) (expected_type (ptr type_def)) (expr (ptr expr)))"),
	type_of3);
  defun("check-type-run?", ("(fcn bool)"), is_check_type_run);
  defun("print-expr", ("(fcn void (theexpr (ptr expr)))"), print_expr);
  defun("ptr-inner", ("(fcn (ptr type_def) (ptr (ptr type_def)))"),  ptr_inner);
  defun("type2expr", ("(fcn (ptr expr) (t (ptr type_def)))"), type2expr);
  defun("symbol-name", ("(fcn (ptr char) (sym (ptr symbol)))"), symbol_name2);
  defun("var-type", ("(fcn (ptr type_def) (variable (ptr symbol)))"), var_type);
  defun("get-var", ("(fcn (ptr void) (sym (ptr symbol)))"), get_var);
  defun("is-fcn-type?", ("(fcn bool (type (ptr type_def)))"), is_fcn_type);
  defun("is-ptr-type?", ("(fcn bool (type (ptr type_def)))"), is_ptr_type);
  defun("is-integer-type?", ("(fcn bool (type (ptr type_def)))"), is_integer_type);
  defun("is-float-type?", ("(fcn bool (type (ptr type_def)))"), is_float_type);
  defun("fcn-arg-types", ("(fcn (ptr (ptr type_def)) (t (ptr type_def)))"), fcn_arg_types);
  defun("fcn-ret-type", ("(fcn (ptr type_def) (t (ptr type_def)))"), fcn_ret_type);
  defun("fcn-arg-cnt", ("(fcn u64 (t (ptr type_def)))"), fcn_arg_cnt);
  defun("set-printer", ("(fcn void (ptr (ptr symbol)))"), set_printer);
  str2type("(alias (opaque-struct _ccdispatch) ccdispatch)");
  defun("timestamp", ("(fcn i64)"), timestamp);
}
