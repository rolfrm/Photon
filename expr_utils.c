#include <stdio.h>
#include <iron/full.h>
#include "lisp_parser.h"
#include "lisp_types.h"
#include "c_ast.h"
#include "lisp_compiler.h"
#include "lisp_std_types.h"
#include "expr_utils.h"
expr * vexpr_symbol(expr e){
  return intern_expr(&e);
}

expr * expr_symbol(expr e){
  return intern_expr(&e);
}

expr symbol_expr(char * name){
  expr e;
  e.type = VALUE;
  e.value = name;
  return e;
}

expr symbol_expr2(symbol name){
  return symbol_expr(symbol_name(name));
}

expr string_expr(char * name){
  return  symbol_expr2(get_symbol_fmt("\"%s\"",name));
}

bool is_symbol(expr exp){
  return exp.type == VALUE;
}

bool is_string(expr exp){
  return exp.type == VALUE;
}

bool is_keyword(expr exp){
  return exp.type == VALUE;
}

char * read_symbol(expr name){
  return name.value;
}
