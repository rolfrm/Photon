#include <stdio.h>
#include <iron/full.h>
#include "lisp_parser.h"
#include "lisp_types.h"
#include "c_ast.h"
#include "lisp_compiler.h"
#include "lisp_std_types.h"
#include "expr_utils.h"
symbol vexpr_symbol(value_expr e){
  char buf[e.strln + 1];
  buf[e.strln] = 0;
  memcpy(buf, e.value, e.strln);
  return get_symbol(buf);
}

symbol expr_symbol(expr e){
  return vexpr_symbol(e.value);
}

expr symbol_expr(char * name){
  expr e;
  e.type = VALUE;
  e.value.type = SYMBOL;
  e.value.value = name;
  e.value.strln = strlen(name);
  return e;
}

expr symbol_expr2(symbol name){
  return symbol_expr(symbol_name(name));
}

expr string_expr(char * name){
  expr e = symbol_expr(name);
  e.value.type = STRING;
  return e;
}

bool is_symbol(expr exp){
  return exp.type == VALUE && (exp.value.type == SYMBOL);
}

bool is_string(expr exp){
  return exp.type == VALUE && exp.value.type == STRING;
}

bool is_keyword(expr exp){
  return exp.type == VALUE && exp.value.type == KEYWORD;
}

char * read_symbol(expr name){
  return fmtstr("%.*s",name.value.strln, name.value.value);
}
