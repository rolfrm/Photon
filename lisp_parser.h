// requires .
typedef enum{
  EXPR = 5,
  VALUE = 6,
  ERROR
}expr_type;

typedef struct _expr expr;

typedef struct{
  expr * exprs;
  size_t cnt;  
}sub_expr;

struct _expr{
  expr_type type;
  union{
    sub_expr sub_expr;
    char * value;
  };
};

// Parses a number of expers no bigger than the start value of *in_out_exprs_count.
// returns the number of parsed expres in in_out_exprs_count. returns where the parser got to.
char * lisp_parse(char * code, expr * out_exprs, int * in_out_exprs_count);

// Deletes an expression tree. Used for cleanup.
void delete_expr(expr * expr);

// prints an expression tree.
void print_expr(expr * expr);

// convenience
expr * lisp_parse_all(char * code, size_t * out_cnt);
expr lisp_parse1(char * code);
expr * clone_expr(expr * e);
expr clone_expr2(expr tree);
char * expr_to_string(expr e);
// test
bool test_lisp_parser();

// parsing
bool is_alphanum(char c);
int is_hex(char c);
