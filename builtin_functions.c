#include <iron/full.h>
#include "lisp_parser.h"
#include "lisp_types.h"
#include "lisp_compiler.h"
#include "expr_utils.h"
void defun(char * name, type_def * t, void * fcn){
  compiler_define_variable_ptr(get_symbol(name), t, fcn);
}

void print_type(type_def * def){
  logd("type: '"); print_def(def); logd("'\n");
}

void write_line(char * str){
  logd("%s\n", str);
}

type_def * ptr_inner(type_def * ptr_def){
  ASSERT(ptr_def->type == POINTER);
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

f32 f32_add(f32 a, f32 b){ return a + b; }
f32 f32_sub(f32 a, f32 b){ return a - b; }
f32 f32_mul(f32 a, f32 b){ return a * b; }
f32 f32_div(f32 a, f32 b){ return a / b; }

i32 i32_add(i32 a, i32 b) { return a + b; }

type_def * type_of(expr * ex){
  c_block blk;
  blk.exprs = NULL;
  blk.expr_cnt = 0;
  c_value val;
  //u64 start = timestamp();
  type_def * otype = _compile_expr(&blk, &val, *ex);
  //u64 stop = timestamp();
  //print_expr(ex);
  //  logd("Type of: %f s\n", 1e-6 * (stop - start));
  return otype;
}

char * symbol_name2(symbol * sym){
  return symbol_name(*sym);
}

bool is_fcn_type(type_def * t){
  return t->type == FUNCTION;
}

type_def ** fcn_arg_types(type_def * td){
  if(td->type != FUNCTION) return NULL;
  return td->fcn.args;
}

u64 fcn_arg_cnt(type_def * td){
  ASSERT(td->type == FUNCTION);
  return td->fcn.cnt;
}

extern symbol * printer;
void set_printer(symbol * sym){
  printer = sym;  
}

bool is_type_compatible2(type_def * call_type, type_def * arg_type, expr * exp){
  return is_type_compatible(call_type, arg_type, *exp);
}

type_def * var_type(symbol * sym){
  var_def * var = get_variable(*sym);
  if(var != NULL)
    return var->type;
  return NULL;
}

void * get_var(symbol * sym){
  var_def * var = get_variable(*sym);
  if(var == NULL) return NULL;
  return var->data;
}

void invoke (void (* fcn)()){
  logd("hello!\n");
  void (*f)() = fcn;
  f();
}

void load_functions(){
  defun("print-type", str2type("(fcn void (a (ptr type_def)))"), print_type);
  type_def * i64fcn_def = str2type("(fcn i64 (a i64) (b i64))");
  defun("i64+", i64fcn_def, &i64_add);
  defun("i64-", i64fcn_def, &i64_sub);
  defun("i64*", i64fcn_def, &i64_mul);
  defun("i64/", i64fcn_def, &i64_div);

  type_def * u64fcn_def = str2type("(fcn u64 (a u64) (b u64))");
  defun("u64+", u64fcn_def, &u64_add);
  defun("u64-", u64fcn_def, &u64_sub);
  defun("u64*", u64fcn_def, &u64_mul);
  defun("u64/", u64fcn_def, &u64_div);

  type_def * u32fcn_def = str2type("(fcn u32 (a u32) (b u32))");
  defun("u32+", u32fcn_def, &u32_add);
  defun("u32-", u32fcn_def, &u32_sub);
  defun("u32*", u32fcn_def, &u32_mul);
  defun("u32/", u32fcn_def, &u32_div);

  type_def * f32fcn_def = str2type("(fcn f32 (a f32) (b f32))");
  defun("f32+", f32fcn_def, &f32_add);
  defun("f32-", f32fcn_def, &f32_sub);
  defun("f32*", f32fcn_def, &f32_mul);
  defun("f32/", f32fcn_def, &f32_div);

  defun("get-symbol", str2type("(fcn (ptr symbol) (a (ptr char)))"), get_symbol2);
  
  type_def * d2t =  str2type("(fcn f64 (a f64) (b f64))");
  defun("f+", d2t, double_add);
  defun("f-", d2t, double_sub);
  defun("f/", d2t, double_div);
  defun("f*", d2t, double_mul);

  defun("size-of",str2type("(fcn u64 (type (ptr type_def)))"), size_of);
  str2type("(alias (ptr (opaque-struct _lib)) lib)"); // declare the lib tyoedef
  defun("load-lib",str2type("(fcn lib (libname (ptr char)))"), load_lib);
  type_def * loadsymbol = str2type("(fcn (ptr void) (_lib lib) (sym (ptr symbol)) (name (ptr symbol)) (t (ptr type_def)))");
  defun("load-symbol", loadsymbol, load_symbol);
  defun("type-of",str2type("(fcn (ptr type_def) (expr (ptr expr)))"), type_of);
  defun("print-expr", str2type("(fcn void (theexpr (ptr expr)))"), print_expr);
  defun("ptr-inner", str2type("(fcn (ptr type_def) (ptr (ptr type_def)))"),  ptr_inner);
  defun("type2expr", str2type("(fcn (ptr expr) (t (ptr type_def)))"), type2expr);
  defun("symbol-name", str2type("(fcn (ptr char) (sym (ptr symbol)))"), symbol_name2);
  defun("var-type", str2type("(fcn (ptr type_def) (variable (ptr symbol)))"), var_type);
  defun("get-var", str2type("(fcn (ptr void) (sym (ptr symbol)))"), get_var);
  defun("is-fcn-type?", str2type("(fcn bool (type (ptr type_def)))"), is_fcn_type);
  defun("fcn-arg-types", str2type("(fcn (ptr (ptr type_def)) (t (ptr type_def)))"), fcn_arg_types);
  defun("fcn-arg-cnt", str2type("(fcn u64 (t (ptr type_def)))"), fcn_arg_cnt);
  defun("set-printer", str2type("(fcn void (ptr (ptr symbol)))"), set_printer);
  defun("is-type-compatible", str2type("(fcn bool (call-type (ptr type_def)) (arg-type (ptr type_def)) (call-expr (ptr expr)))"), is_type_compatible2);
  defun("invoke", str2type("(fcn void (func (fcn void )))"), invoke);
}
