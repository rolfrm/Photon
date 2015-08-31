#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <stdint.h>
#include <stdarg.h>
#include "lisp_parser.h"
#include <stdio.h>
#include <iron/full.h>
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
  return c == ')' || c == '(' || is_whitespace(c) || c == 0 || c ==';';
}

bool is_keyword_char(char c){
  return !is_endexpr(c);
}

char * parse_symbol(char * code, expr * sym){
  char * end = take_while(code,is_keyword_char);
  if(end == code)
    return NULL;
  if(!is_endexpr(*end))
    return NULL;
  sym->value = fmtstr("%.*s",end - code, code);
  sym->type = VALUE;
  return end;
}

// assuming we are standing on the first ", this will return the position after the next '"', except if it is iscaped by \\.
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
  ASSERT("this is an error");
  return NULL;
}

char * parse_string(char * code, expr * string){
  if(*code != '"') return NULL;
  char * end = read_to_end_of_string(code);
  string->value = fmtstr("%.*s",end - code, code);
  string->type = VALUE;
  return end;
}

int is_hex(char chr){
  switch(chr){
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':  
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
  case 'a':
  case 'b':
  case 'c':
  case 'd':
  case 'e':
  case 'f':
  case 'A':
  case 'B':
  case 'C':
  case 'D':
  case 'E':
  case 'F':
    return 1;
  default:
    return 0;
  }
}
char * parse_single_line_comment(char * code){
  bool is_comment(char c){
    return c != '\n';
  }
  if(*code != ';')
    return NULL;
  return take_while(code, is_comment) + 1;
}

char * parse_value(char * code, expr * val){
  char * next;
  next = parse_string(code, val);
  if(next != NULL) return next;
  next = parse_symbol(code, val);
  if(next != NULL) return next;
  return NULL;
}
char * parse_expr(char * code, expr * out_expr);


char * parse_subexpr(char * code, sub_expr * subexpr){
  expr exprs[100];
  if(code[0] != '(')
    return NULL;
  code++;
  code = take_while(code,is_whitespace);
  subexpr->exprs = NULL;
  subexpr->cnt = 0;
  size_t len = 0;

 next_part:
  code = take_while(code,is_whitespace);
  
  char * commentskip = parse_single_line_comment(code);
  if(commentskip != NULL){
    code = commentskip;
    goto next_part;
  }

  if(*code == ')') {
    subexpr->exprs = clone(exprs,sizeof(expr) * len);
    subexpr->cnt = len;
    return code + 1;

  }  
  
  expr e;
  
    
  code = parse_expr(code, &e);
  
  if(code == NULL){
    subexpr->cnt = 0;
    return NULL;
  }

  exprs[len] = e;
  len++;
  
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
    
  { // parse value.  
    char * next = parse_value(code, out_expr);
    if(next != NULL)
      return next;
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
  }else if(expr->type == VALUE){
    dealloc(expr->value);
  }
}

void print_expr(expr * expr1){
  void iprint(expr * expr2, int indent){
    sub_expr subexpr = expr2->sub_expr;
    
    switch(expr2->type){
    case EXPR:
      format("(");
      for(size_t i = 0 ; i < subexpr.cnt; i++){
	iprint(subexpr.exprs + i,indent + 1);
	if(i != (subexpr.cnt - 1)) logd(" ");
      }
      format(")");
      break;
    case VALUE:
      format("%s", expr2->value);
      break;
    case ERROR:
      logd("(Parser Error)");
      break;
    }
  }
  iprint(expr1,0);
}


char * lisp_parse(char * code, expr * out_expr){
  while(true){
    code = take_while(code, is_whitespace);
    char * commentskip = parse_single_line_comment(code);
    if(commentskip != NULL){
      code = commentskip;
    }else break;
  }
  return parse_expr(code, out_expr);   
}

char * expr_to_string(expr e){
  char buf[100];
  static int tmp_idx = 0;
  sprintf(buf, "__tmp_expr_file%i", tmp_idx++);
  FILE * f = fopen(buf,"wb+");
  push_format_out(f);
  print_expr(&e);
  pop_format_out();
  char * data = read_stream_to_string(f);
  fclose(f);
  remove(buf);
  return data;
}

expr clone_expr2(expr body){
  if(body.type == VALUE){
    body.value = clone(body.value,strlen(body.value) + 1);
    return body;
  }
  sub_expr exp = body.sub_expr;
  expr * sub = alloc0(sizeof(expr) * exp.cnt);
  for(size_t i = 0; i < exp.cnt; i++)
    sub[i] = clone_expr2(exp.exprs[i]);
  
  expr nexpr;
  nexpr.type = EXPR;
  nexpr.sub_expr.exprs = sub;
  nexpr.sub_expr.cnt = exp.cnt;
  return nexpr;
}

expr * clone_expr(expr * tree){
  expr * root = alloc(sizeof(expr));
  *root = clone_expr2(*tree);
  return root;
}

// static bool test_infinite_bug(){
//   // this turned out to not be a bug.
//   char * code = "(print_string \"Hello World\\n\")(glfwInit)(print_string (glfwGetVersionString))";
//   char * next = code;
//   expr out_expr[2];
//   while(next != NULL && *next != 0){
//     int out = array_count(out_expr);
//     char * prev = next;
//     next = lisp_parse(next,out_expr,&out);
//     TEST_ASSERT(prev != next);
//   }
//   return TEST_SUCCESS;
// }

// bool test_empty(){
//   char code[] = ";hello world\n;hello world\n\n;asd\n\n\n";
//   size_t out_cnt = 0;
//   expr * exprs = lisp_parse_all(code, &out_cnt);
//   TEST_ASSERT(out_cnt == 0 && exprs == NULL);
//   return TEST_SUCCESS;
// }

// bool test_type_bug(){
//   char * code = "(alias (struct _s1 (x i16) (y i8) (z i16) (x2 i16) (y2 i8) (z2 i16) (x3 i16) (y3 i8) (z3 i16)) s1)";
//   size_t out_cnt = 0;
//   expr * exprs = lisp_parse_all(code, &out_cnt);
//   ASSERT(exprs->type == EXPR);
//   return TEST_SUCCESS;
// }

// bool test_lisp_parser(){

//   TEST(test_empty);
//   expr exprs[10];
//   int exprs_count = 10;

//   lisp_parse("(hej (hej2 1.0312))(add (sub 1 :a 5  \"hello\") 2) -1\n",exprs,&exprs_count);
//   TEST_ASSERT(exprs_count == 3);
//   //TEST_ASSERT(exprs[2].type == VALUE && exprs[2].value.type == NUMBER && exprs[2].value.value[0] == '-');
//   for(int i = 0; i < exprs_count; i++)
//     delete_expr(exprs + i);
//   TEST(test_infinite_bug);
//   TEST(test_type_bug);
//   return TEST_SUCCESS;
// }



