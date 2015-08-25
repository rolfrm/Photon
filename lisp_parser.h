// requires .
typedef enum{
  EXPR = 5,
  VALUE = 6,
  ERROR
}expr_type;

typedef enum{
  // All numbers.
  NUMBER = 1,
  // things starting with ':'
  KEYWORD = 2,
  // "-delimited strings
  STRING = 3,
  // Comments are ignored by the compiler
  COMMENT = 4,
  // Symbols are used first in functions.
  SYMBOL = 5
}value_type;

typedef struct{
  
}number_expr;

typedef enum _number_kind_enum{
  LISP_INTEGER,
  LISP_FLOAT,
  LISP_HEX,
  LISP_BINARY
}number_kind_enum;

typedef struct{
  value_type type;
  char * value;
  size_t strln;
  number_kind_enum number_kind;
}value_expr;

typedef struct _expr expr;

typedef struct{
  expr * exprs;
  size_t cnt;  
}sub_expr;

struct _expr{
  expr_type type;
  union{
    sub_expr sub_expr;
    value_expr value;
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
