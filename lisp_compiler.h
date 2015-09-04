// requires bitguy.h, lisp_parser.h, lisp_types

// variable definition
typedef struct _var_def{
  // Name of the variable
  expr * name;
  // Type of the variable
  type_def * type;
  // Pointer to the variable data.
  void * data;
  // true if the pointer to the variable data already as a pointer.
  // not completely related to the 'type'.
  bool is_ptr;
} var_def;

// Struct for holding the current compiler state. Structure for containing global variables.
struct _compiler_state;
typedef struct _compiler_state compiler_state;

// Structure for storing macros defined in c.
typedef struct{
  // -1 if varadic macro.
  i64 arg_cnt;
  // Pointer to the macro pointer.
  void * fcn;
}cmacro_def;

typedef enum {
  COMPILE_OK,
  COMPILE_ERROR
}compile_status;

extern bool lisp_print_errors;
bool is_check_type_run();
#define COMPILE_ASSERT(expr) if(!(expr)){ if(lisp_print_errors){loge("at %s : %i:", __FILE__, __LINE__);loge("Compile error '" #expr "' at %s: %l", __FILE__, __LINE__ ); logd("\n");}return error_def;}

#define COMPILE_ERROR(fmt, ...) {if(lisp_print_errors){loge("at %s : %i: ", __FILE__, __LINE__);ERROR(fmt,##__VA_ARGS__);logd("\n"); }return error_def;}

#define CHECK_TYPE(expected, required) if(expected != NULL && expected != &void_def && required != expected) (COMPILE_ERROR("Unsupported type"));

// allocates a new lisp compiler. Delete with dealloc.
compiler_state * lisp_make_compiler();

// Used for setting the current compiler.
extern compiler_state * lisp_current_compiler;

// *DEBUG* Sets the output file for the C compiler. set path=NULL to disable.
void set_compile_out(compiler_state * compiler, char * path);

// *DEBUG* Gets current compile out.
char * get_compile_out(compiler_state * compiler);

// Compile expression return the new variable (result).
var_def * lisp_compile_expr(expr * ex, compile_status * opt_outstatus);

// same as above except it returns the result
compile_status lisp_run_expr(expr * ex);

// loads lisp base
void lisp_load_base(char * init_directory);

// Runs a number of Lisp expressions.
compile_status lisp_run_exprs(expr * exprs, size_t exprcnt);

// Runs a string of Lisp code.
compile_status lisp_run_script_string(char * code);

// Runs a Lisp code file.
compile_status lisp_run_script_file(char * filepath);

// defines a variable pointer. ptr is a pointer to the data of type t.
//void compiler_define_variable_ptr(symbol sym, type_def * t, void * ptr);

// Defines / declares a macro.
void define_macro(char * name, int nargs, void * fcn);

// registers a type with the compiler.
void compiler_reg_type(compiler_state *c, symbol name, type_def * t);

// Loads the Lisp std types
void compiler_load_types(compiler_state *);

// returns the global variable named s. Returns NULL if it does not exist.
var_def * get_global(expr * s);

// Finds a variable on the stack. Data will usually be NULL for these.
var_def * get_stack_variable(expr * name);

// Finds a variable on the stack or globally. It searches the stack from the top and last the global scope.
var_def * get_any_variable(expr * name);
void define_variable(expr * name, type_def * type, void * data, bool is_ptr);
 
// writes out the dependencies for deps as c code.
void write_dependencies(type_def ** deps);

// Gets a c-compatible version of the name of s.
// Warning: returned pointer is only valid untill next time get_c_name is called.
char * get_c_name(expr * s);

// Prints get_c_name out.
void format_c_name(expr * s);

i64 interned_index(expr * e);

// Gets the type from typexprs.
type_def * expr2type(expr typexpr);

// Gets the return type of an expression.
type_def * type_of(expr * ex);
// Gets the return type of an expression. Expected type can be NULL if dont care. returned type might be different from expected_type although this is usually an error.
type_def * type_of2(type_def * expected_type, expr * ex);


// Creates a new type from the string 'str'. for example "(struct x2 (x i64) (x_2 i64))".
type_def * str2type(char * str);

// Compiles an expression and returns the type.
// expected type can be NULL if unknown or dont care.
type_def * compile_expr(type_def * expected_type, c_block * block, c_value * val,  expr * e );

// Compiles the 'codes'. The newly available functions variables will be available in the compiler variables. returns the buffer for the code. This buffer can be removed unless any of the symbols defined are needed.
void * compile_as_c(c_root_code * codes, size_t code_cnt);

// Compiles a value expression. Returns the type.
// expected type can be NULL if unknown or dont care.
type_def * compile_value(type_def * expected_type, c_value * val, expr * e);

// Defines a new function named 'sym', t and fcnptr should match. see example in builtin_functions.
void defun(char * sym, char * type, void * fcnptr);

// returns the macrostore type.
type_def * macro_store_type();

// Expand he macro as defined in exprs.
type_def * expand_macro(type_def * expected_type, c_block * block, c_value * val, expr ** exprs, size_t cnt);

// Empty/null symbol
extern const symbol symbol_empty;

// Interns the string 'name' as a new symbol.
expr * get_symbol(char * name);

// Interns the formatted string as a new symbol.
expr * get_symbol_fmt(char * fmt, ...);

// Interns the string 'name' as a new symbol.
//symbol * get_symbol2(char * name);

// Returns the interned name of the symbol 's'. The returned char * is guaranteed to be the same pointer if the symbol is the same.
char * symbol_name(expr * s);

// Compares two symbols. Returns true if equals.
//bool symbol_cmp(symbol a, symbol b);

// Pushes a symbol array on the symbol stack. Used for new scopes. *vars is allowed to change while on the stack.
void push_symbols(var_def ** vars, size_t * vars_cnt);

// Pops the top stack from the list of symbols.
void pop_symbols();
expr * intern_expr(expr * e);
// generates a unique symbol.
expr * gensym();
bool is_number_literal(expr ex);
void add_delete_soon(void * buffer);
void print_current_mem();
// Testing
bool test_lisp2c();
void checktypepool();
