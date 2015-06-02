// requires bitguy.h, lisp_parser.h, lisp_types

typedef struct _fcn_def{
  char * name;
  type_def * type;
  u8 is_extern;
  void * ptr;
}fcn_def;

typedef struct{
  char * name;
  type_def * type;
  void * data;
}var_def;

// Currently the compiler just contains variables.
struct _compiler_state{
  var_def * vars;
  size_t var_cnt;
};

typedef struct _compiler_state compiler_state;

typedef struct{
  compiler_state * c;
  char * buffer;
  // required functions
  //fcn_def * fcns;
  //size_t fcn_cnt;
  char ** deps;
  size_t dep_cnt;
}comp_state;

typedef struct{
  type_def result_type;
  void * fcn;
}compiled_expr;

typedef struct{
  char * name;
  i64 arg_cnt;
  void * fcn;
}cmacro_def;

compiler_state * get_compiler();
compiler_state * compiler_make();
void lisp_load_compiler(compiler_state * c);
void * compiler_define_variable(char * name, type_def * t);
void lisp_run_exprs(compiler_state * c, expr * exprs, size_t exprcnt);

// defines a variable pointer.
void compiler_define_variable_ptr(char * name, type_def * t, void * ptr);
void define_macro(char * name, int nargs, void * fcn);

void compiler_reg_type(compiler_state *c, char * name, type_def * t);
void compiler_load_types(compiler_state *);

bool fcn_def_cmp(fcn_def a, fcn_def b);
comp_state comp_state_make();
var_def * get_variable(char * name, size_t name_len);
var_def * get_variable2(char * name);
fcn_def * get_fcn_def(char * name, size_t name_len);
cmacro_def * get_cmacro_def(char * name, size_t name_len);
compiled_expr compile_expr(expr * e);
type_def compile_iexpr(expr expr1);
void compiler_set_state(compiler_state * ls);
void write_dependencies(type_def ** deps);
void with_compiler(compiler_state * c, void (* fcn)());

void lisp_run_script_file(compiler_state * c, char * filepath);

char * get_c_name(char * sym);
// symbols
void with_symbols(var_def ** vars, size_t * vars_cnt, void (*fcn)());

bool test_lisp2c();
