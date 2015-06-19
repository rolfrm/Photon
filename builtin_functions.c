#include <iron/full.h>
#include "lisp_parser.h"
#include "lisp_types.h"
#include "lisp_compiler.h"

void defun(char * name, type_def * t, void * fcn){
  compiler_define_variable_ptr(get_symbol(name), t, fcn);
}

void print_type(type_def * def){
  logd("type: '")print_def(def); logd("'\n");
}

void write_line(char * str){
  logd("%s\n", str);
}

#include <dlfcn.h>
void * load_lib(char * path){
  return dlopen(path, RTLD_LAZY);
}

void * load_symbol(void * lib, symbol * sym, symbol * name, type_def * t){
  void * ptr = dlsym(lib, get_c_name(*name));
  if(ptr == NULL) {
    loge("Error no such symbol '%s'", symbol_name(*name));}
  else {compiler_define_variable_ptr(*sym, t, ptr);}
  return ptr;
}

double double_add(double a, double b){ return a + b;}
double double_sub(double a, double b){ return a - b;}
double double_div(double a, double b){ return a / b;}
double double_mul(double a, double b){ return a * b;}
i64 i64_add(i64 a, i64 b){ return a + b; }
i32 i32_add(i32 a, i32 b) { return a + b; }
void load_functions(){
  defun("print_type", str2type("(fcn void (a (ptr type_def)))"), print_type);
  defun("write_line", str2type("(fcn void (a (ptr char)))"), &write_line);
  defun("i64+", str2type("(fcn i64 (a i64) (b i64))"), &i64_add);
  defun("i32+", str2type("(fcn i32 (a i32) (b i32))"), &i32_add);
  defun("get-symbol", str2type("(fcn (ptr symbol) (a (ptr char)))"), get_symbol2);
  
  type_def * d2t =  str2type("(fcn f64 (a f64) (b f64))");
  defun("f+", d2t, double_add);
  defun("f-", d2t, double_sub);
  defun("f/", d2t, double_div);
  defun("f*", d2t, double_mul);
  defun("size-of",str2type("(fcn u64 (type (ptr type_def)))"), &size_of);
  str2type("(alias (ptr (opaque-struct _lib)) lib)"); // declare the lib tyoedef
  defun("load-lib",str2type("(fcn lib (libname (ptr char)))"), &load_lib);
  type_def * loadsymbol = str2type("(fcn (ptr void) (_lib lib) (sym (ptr symbol)) (name (ptr symbol)) (t (ptr type_def)))");
  defun("load-symbol", loadsymbol, &load_symbol);
}
