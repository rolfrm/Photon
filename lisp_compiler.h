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

typedef enum {
  COMPILE_OK,
  COMPILE_ERROR
}compile_status;

#define COMPILE_ASSERT(expr) if(!(expr)){loge("at %s : %i:", __FILE__, __LINE__);loge("Compile error '" #expr "' at %s: %l", __FILE__, __LINE__ ); logd("\n");return &error_def;}

#define COMPILE_ERROR(fmt, ...) {loge("at %s : %i: ", __FILE__, __LINE__);loge(fmt,##__VA_ARGS__); logd("\n"); return &error_def;}

// Get current compiler.
compiler_state * get_compiler();

// Create a new compiler.
compiler_state * compiler_make();

// Todo: Why do I need a stack of compilers?

// Pushes the compiler c onto the stack.
void push_compiler(compiler_state * c);

// pops the compiler from the stack.
void pop_compiler();

// Compile expression return the new variable (result).
var_def * lisp_compile_expr(expr ex, compile_status * opt_outstatus);

// same as above except it returns the result
void * lisp_compile_and_run_expr(expr ex, compile_status * opt_outstatus);

// loads lisp base
void lisp_load_base();

// Runs a number of Lisp expressions.
compile_status lisp_run_exprs(expr * exprs, size_t exprcnt);

// Runs a string of Lisp code.
compile_status lisp_run_script_string(char * code);

// Runs a Lisp code file.
compile_status lisp_run_script_file(char * filepath);

// defines a variable pointer. ptr is a pointer to the data of type t.
void compiler_define_variable_ptr(symbol sym, type_def * t, void * ptr);

// Defines / declares a macro.
void define_macro(char * name, int nargs, void * fcn);

// registers a type with the compiler.
void compiler_reg_type(compiler_state *c, symbol name, type_def * t);

// Loads the Lisp std types
void compiler_load_types(compiler_state *);

// Creates a new compiler state
comp_state comp_state_make();

// warning: The returned variable will eventually get invalidated by defining new variables.
// returns the variable named s. Returns NULL if it does not exist.
var_def * get_variable(symbol s);

// Returns true if call_type is equal to arg_type or if callexpr is a literal number and call_type takes a number.
bool is_type_compatible(type_def * call_type, type_def * arg_type, expr callexpr);

// writes out the dependencies for deps as c code.
void write_dependencies(type_def ** deps);

// Gets a c-compatible version of the name of s.
// Warning: returned pointer is only valid untill next time get_c_name is called.
char * get_c_name(symbol s);

// Prints get_c_name out.
void format_c_name(symbol s);

// Gets the type from typexprs.
type_def * expr2type(expr typexpr);

// Gets the return type of an expression.
type_def * type_of(expr * ex);

// compiles an expression and returns the type.
type_def * compile_expr(c_block * block, c_value * val,  expr e );


void compile_as_c(c_root_code * codes, size_t code_cnt);
type_def * compile_value(c_value * val, value_expr e);

void defun(char * sym, type_def * t, void * fcnptr);
type_def * macro_store_type();
type_def * expand_macro(c_block * block, c_value * val, expr * exprs, size_t cnt);
// symbols
void with_symbols(var_def ** vars, size_t * vars_cnt, void (*fcn)());
void push_symbols(var_def ** vars, size_t * vars_cnt);
void pop_symbols();

bool test_lisp2c();
void checktypepool();
