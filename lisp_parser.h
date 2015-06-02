// requires nothing.
typedef enum{
  EXPR = 5,
  VALUE = 6
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
  SYMBOL = 5,
  VOID = 6,
  CVARADIC = 7, // c: ...
  VALUE_TYPE_LAST
}value_type;

typedef struct{
  value_type type;
  char * value;
  size_t strln;
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

char * lisp_parse(char * code, expr * out_exprs, int * out_exprs_count);
void delete_expr(expr * expr);
void print_expr(expr * expr);
bool test_lisp_parser();

expr * lisp_parse_all(char * code, size_t * out_cnt);
expr lisp_parse1(char * code);
