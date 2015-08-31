// requires .
typedef enum{
  // The expression is a sub-expression.
  EXPR = 5,
  // The expression is a value (symbol or string)
  VALUE = 6,
  // Used in case of parser errors.
  ERROR
}expr_type;

typedef struct _expr expr;

//Sub expression type.
typedef struct{
  // Elements. Might be null if cnt == 0.
  expr * exprs;
  // Number of elements in exprs.
  size_t cnt;  
}sub_expr;

// Expression type. Can be either a sub expression or value.
struct _expr{
  expr_type type;
  union{
    sub_expr sub_expr;
    char * value;
    struct{
      expr * exprs;
      size_t cnt;
    };
    void * payload;
  };
};

// Parses a number of expers no bigger than the start value of *in_out_exprs_count.
// returns the number of parsed expres in in_out_exprs_count. returns where the parser got to.
char * lisp_parse(char * code, expr * out_expr);

// Deletes an expression tree. Used for cleanup.
void delete_expr(expr * expr);

// prints an expression tree.
void print_expr(expr * expr);

// Clones an expression tree.
expr * clone_expr(expr * e);
expr clone_expr2(expr tree);
char * expr_to_string(expr e);
// test
bool test_lisp_parser();

// parsing
// returns true if c is alphanumeric
bool is_alphanum(char c);
// returns true if c is a hex character 0-9A-Fa-f.
int is_hex(char c);
