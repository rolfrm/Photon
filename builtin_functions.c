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
i64 i64_sub(i64 a, i64 b){ return a - b; }
i64 i64_mul(i64 a, i64 b){ return a * b; }
i64 i64_div(i64 a, i64 b){ return a / b; }

u64 u64_add(u64 a, u64 b){ return a + b; }
u64 u64_sub(u64 a, u64 b){ return a - b; }
u64 u64_mul(u64 a, u64 b){ return a * b; }
u64 u64_div(u64 a, u64 b){ return a / b; }

u32 u32_add(u32 a, u32 b){ return a + b; }
u32 u32_sub(u32 a, u32 b){ return a - b; }
u32 u32_mul(u32 a, u32 b){ return a * b; }
u32 u32_div(u32 a, u32 b){ return a / b; }

i32 i32_add(i32 a, i32 b) { return a + b; }
type_def * type_of(expr * ex){
c_block blk;
blk.exprs = NULL;
blk.expr_cnt = 0;
c_value val;
return _compile_expr(&blk, &val, *ex);
}

void load_functions(){
  defun("print_type", str2type("(fcn void (a (ptr type_def)))"), print_type);
  defun("write_line", str2type("(fcn void (a (ptr char)))"), &write_line);
  type_def * i64fcn_def = str2type("(fcn i64 (a i64) (b i64))");
  defun("i64+",i64fcn_def , &i64_add);
  defun("i64-",i64fcn_def , &i64_sub);
  defun("i64*",i64fcn_def , &i64_mul);
  defun("i64/",i64fcn_def , &i64_div);

  type_def * u64fcn_def = str2type("(fcn u64 (a u64) (b u64))");
  defun("u64+",u64fcn_def , &u64_add);
  defun("u64-",u64fcn_def , &u64_sub);
  defun("u64*",u64fcn_def , &u64_mul);
  defun("u64/",u64fcn_def , &u64_div);

  type_def * u32fcn_def = str2type("(fcn u32 (a u32) (b u32))");
  defun("u32+",u32fcn_def , &u32_add);
  defun("u32-",u32fcn_def , &u32_sub);
  defun("u32*",u32fcn_def , &u32_mul);
  defun("u32/",u32fcn_def , &u32_div);

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
defun("type-of",str2type("(fcn (ptr type_def) (expr (ptr expr)))"), type_of);
defun("print-expr", str2type("(fcn void (expr (ptr expr)))"), print_expr);
}
