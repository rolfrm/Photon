// requires lisp_parser.h, lisp_types.h, stdbool.h

symbol vexpr_symbol(value_expr e);
symbol expr_symbol(expr e);
expr symbol_expr(char * name);
expr symbol_expr2(symbol name);
expr string_expr(char * name);
bool is_symbol(expr exp);
bool is_string(expr exp);
bool is_keyword(expr exp);
char * read_symbol(expr name);
