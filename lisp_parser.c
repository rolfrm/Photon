#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <stdint.h>

#include "lisp_parser.h"

#include <iron/types.h>
#include <iron/log.h>
#include <iron/test.h>
#include <iron/mem.h>
#include <iron/array.h>
char * take_while(char * data, bool (* fcn)(char char_code)){
  while(fcn(data[0])) data++;
  return data;
}

bool is_alphanum(char c){
  return isdigit(c) || isalpha(c);
}

bool is_whitespace(char c){
  return (bool)isspace(c) || c == '\n';
}

bool is_endexpr(char c){
  return c == ')' || is_whitespace(c) || c == 0 || c ==';';
}

bool is_keyword_char(char c){
  return !is_endexpr(c);
}

char * parse_keyword(char * code, value_expr * kw){
  if(code[0] != ':')
    return NULL;
  code++;
  char * end = take_while(code, &is_keyword_char);
  if(!is_endexpr(*end)){
    return NULL;
  }
  kw->type = KEYWORD;
  kw->value = code;
  kw->strln = (int) (end - code);
  return end;
}

char * parse_symbol(char * code, value_expr * sym){
  char * end = take_while(code,is_keyword_char);
  if(end == code)
    return NULL;
  if(!is_endexpr(*end))
    return NULL;
  sym->value = code;
  sym->strln = (int) (end - code);
  sym->type = SYMBOL;
  return end;
}

char * read_to_end_of_string(char * code){
  while(true){
    code++;
    if(*code == '"')
      return code + 1;
    if(*code == 0)
      return code;
    if(*code == '\\')
      code++;//next item is escaped. skip it.
  }
  //this is an error.
  return NULL;
}

char * parse_string(char * code, value_expr * string){
  if(*code != '"') return NULL;
  code++;
  char * end = read_to_end_of_string(code);
  string->value = code;
  string->strln = (int) (end - code - 1); //-1: last "
  string->type = STRING;
  return end;
}

char * parse_number(char * code, value_expr * string){
  int decimal_reached = 0;
  char * it = code;
  if(*it == '-') it++; //if negative
  for(; false == is_endexpr(*it); it++){
    if(*it == '.'){
      if(decimal_reached)
	return NULL;
      decimal_reached = 1;
    }else if(!isdigit(*it)){
      return NULL;
    }
  }
  size_t l = (it - code);
  if(l == 0) return NULL;
  string->value = code;
  string->type = NUMBER;
  string->strln = l;
  return it;
}

char * parse_single_line_comment(char * code, value_expr * val){
  bool is_comment(char c){
    return c != '\n';
  }
  if(*code != ';')
    return NULL;
  char * r = take_while(code, is_comment);
  val->type = COMMENT;
  val->value = code + 1;
  val->strln = (size_t)(r - val->value);
  return r + 1;
}

char * parse_value(char * code, value_expr * val){
  char * next;
  next = parse_single_line_comment(code, val);
  if(next != NULL) return next;
  next = parse_string(code, val);
  if(next != NULL) return next;
  next = parse_keyword(code, val);
  if(next != NULL) return next;
  next = parse_number(code, val);
  if(next != NULL) return next;
  next = parse_symbol(code, val);
  if(next != NULL) return next;
  
  return NULL;
}
char * parse_expr(char * code, expr * out_expr);

char * parse_subexpr(char * code, sub_expr * subexpr){
  if(code[0] != '(')
    return NULL;
  code++;
  code = take_while(code,is_whitespace);
  subexpr->exprs = NULL;
  subexpr->cnt = 0;
  expr exprs[10];
  int len = 0;

 next_part:
  code = take_while(code,is_whitespace);

  if(*code == ')'){

    subexpr->cnt = len;
    subexpr->exprs = alloc(len * sizeof(expr));
    memcpy(subexpr->exprs, exprs, len * sizeof(expr));

    return code + 1;  
  }
  code = parse_expr(code, exprs + len);
  if(code == NULL)
    return NULL;
  if(exprs[len].type == VALUE && exprs[len].value.type == COMMENT){
    // skip comments
  }else{
    len++;
  }
  goto next_part;
 
}

char * parse_expr(char * code, expr * out_expr){
  code = take_while(code,is_whitespace);
  {// parse subexpr.
    sub_expr subexpr;
    char * next = parse_subexpr(code,&subexpr);
    if(next != NULL){
      out_expr->type = EXPR;
      out_expr->sub_expr = subexpr;
      return next;
    }
  }
  
  value_expr value;
  { // parse value.
    char * next = parse_value(code, &value);
    if(next != NULL){
      out_expr->value = value;
      out_expr->type = VALUE;
      return next;
    }
  }
  return NULL;
}

void delete_expr(expr * expr){
  if(expr->type == EXPR){
    sub_expr sexpr = expr->sub_expr;
    for(size_t i = 0 ; i < sexpr.cnt; i++){
      delete_expr(sexpr.exprs + i);
    }
    dealloc(sexpr.exprs);
  }
}

char * value_type2str(value_type vt){
  switch(vt){
  case NUMBER: return "number";
  case KEYWORD: return "keyword";
  case STRING: return "string";
  case COMMENT: return "comment";
  case SYMBOL: return "symbol";
  }
  ASSERT_UNREACHABLE();
  return NULL;
}

void print_expr(expr * expr1){

  void iprint(expr * expr2, int indent){
    value_expr value = expr2->value;
    sub_expr subexpr = expr2->sub_expr;
    
    switch(expr2->type){
    case EXPR:
      //logd("%-7s: %*s %.*s \n","expr", indent, " ", value.strln, subexpr.name.value);
      for(size_t i = 0 ; i < subexpr.cnt; i++){
	iprint(subexpr.exprs + i,indent + 1);
      }
      break;
    case VALUE:
      logd("%-7s: %*s  |%.*s|\n", value_type2str(value.type), indent, " ", value.strln,value.value);
      break;
    }
  }
  iprint(expr1,0);
}


char * lisp_parse(char * code, expr * out_exprs, int * out_exprs_count){
  size_t expr_cnt = 0;
  size_t outexprcnt = *out_exprs_count;
  while(*code != 0 && expr_cnt != outexprcnt){
    code = take_while(code, is_whitespace);
    expr out_expr;
    char * cn = parse_expr(code, &out_expr);
    if(cn == NULL) goto end;
    code = cn;
    if(out_expr.type == VALUE && out_expr.value.type == COMMENT){
      // skip comment
    }else{
      out_exprs[expr_cnt++] = out_expr;
    }
  }

 end:
  *out_exprs_count = expr_cnt;
  return code;
}

expr * lisp_parse_all(char * code, size_t * out_cnt){
  *out_cnt = 0;
  expr * exprs = NULL;
  expr expr;
  char * next = code;
  while(next != NULL && *next != 0){
    int cnt = 1;
    next = lisp_parse(next, &expr, &cnt);
    if(cnt == 0 || next == NULL)
      break;
    list_add((void **) &exprs, out_cnt, &expr, sizeof(expr));
  }
  return exprs;
}

expr lisp_parse1(char * code){
  int out_cnt = 1;
  expr expr;
  char * next = code;
  next = lisp_parse(next, &expr, &out_cnt);
  if(out_cnt == 0 || next == NULL){
    ERROR("Unable to parse '%s'", code);
  }
  return expr;  
}

expr _clone_expr(expr body){

  if(body.type == VALUE)
    return body;
  sub_expr exp = body.sub_expr;
  expr sub[exp.cnt];
  for(size_t i = 0; i < exp.cnt; i++){
    sub[i] = _clone_expr(exp.exprs[i]);
  }
  expr nexpr;
  nexpr.type = EXPR;
  nexpr.sub_expr.exprs = clone(sub,sizeof(sub));
  nexpr.sub_expr.cnt = exp.cnt;
  return nexpr;
}

expr * clone_expr(expr * tree){
  expr * root = alloc(sizeof(expr));
  *root = _clone_expr(*tree);
  return root;
}

static bool test_infinite_bug(){
  // this turned out to not be a bug.
  char * code = "(print_string \"Hello World\\n\")(glfwInit)(print_string (glfwGetVersionString))";
  char * next = code;
  expr out_expr[2];
  while(next != NULL && *next != 0){
    int out = array_count(out_expr);
    char * prev = next;
    next = lisp_parse(next,out_expr,&out);
    TEST_ASSERT(prev != next);
  }
  return TEST_SUCCESS;
}

bool test_empty(){
  char code[] = ";hello world\n;hello world\n\n;asd\n\n\n";
  size_t out_cnt = 0;
  expr * exprs = lisp_parse_all(code, &out_cnt);
  TEST_ASSERT(out_cnt == 0 && exprs == NULL);
  return TEST_SUCCESS;
}

bool test_lisp_parser(){

  TEST(test_empty);
  expr exprs[10];
  int exprs_count = 10;

  lisp_parse("(hej (hej2 1.0312))(add (sub 1 :a 5  \"hello\") 2) -1\n",exprs,&exprs_count);
  TEST_ASSERT(exprs_count == 3);
  TEST_ASSERT(exprs[2].type == VALUE && exprs[2].value.type == NUMBER && exprs[2].value.value[0] == '-');
  for(int i = 0; i < exprs_count; i++)
    delete_expr(exprs + i);
  TEST(test_infinite_bug);
  return TEST_SUCCESS;
}



