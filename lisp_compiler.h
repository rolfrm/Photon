// requires bitguy.h, lisp_parser.h, lisp_types

typedef struct{
  symbol name;
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
void * compiler_define_variable(symbol sym, type_def * t);
void lisp_run_exprs(compiler_state * c, expr * exprs, size_t exprcnt);

// defines a variable pointer.
void compiler_define_variable_ptr(symbol sym, type_def * t, void * ptr);
void define_macro(char * name, int nargs, void * fcn);

void compiler_reg_type(compiler_state *c, symbol name, type_def * t);
void compiler_load_types(compiler_state *);

comp_state comp_state_make();
var_def * get_variable(symbol s);

cmacro_def * get_cmacro_def(symbol s);
compiled_expr compile_expr(expr * e);
type_def compile_iexpr(expr expr1);
void compiler_set_state(compiler_state * ls);
void write_dependencies(type_def ** deps);
void with_compiler(compiler_state * c, void (* fcn)());

void lisp_run_script_file(compiler_state * c, char * filepath);

char * get_c_name(symbol s);
// symbols
void with_symbols(var_def ** vars, size_t * vars_cnt, void (*fcn)());

bool test_lisp2c();
