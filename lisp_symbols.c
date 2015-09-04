#include <stdio.h>
#include <iron/full.h>
#include "lisp_parser.h"
#include <libtcc.h>
#include "lisp_types.h"
#include "c_ast.h"
#include "lisp_compiler.h"
#include "lisp_std_types.h"
#include <ctype.h>
#include <stdarg.h>
expr * get_symbol(char * name){
  expr e;
  if(name == NULL || lisp_parse(name, &e) == NULL){
    e.type = VALUE;
    e.value = "";
  }
  return intern_expr(&e);
}

expr * get_symbol_fmt(char * fmt, ...){
  ASSERT(fmt != NULL);
  va_list args;
  va_start(args, fmt);
  va_list args2;
  va_copy(args2, args);
  size_t size = vsnprintf (NULL, 0, fmt, args2) + 1;
  va_end(args2);
  char buf[size];
  vsprintf (buf, fmt, args);
  va_end(args);
  return get_symbol(buf);
}

typedef struct _symbol_stack symbol_stack;
struct _symbol_stack{
  var_def ** vars;
  size_t *vars_cnt;
};

static __thread symbol_stack symbolstacks[100];
static size_t stack_count = 0;

void push_symbols(var_def ** vars, size_t * vars_cnt){
  symbolstacks[stack_count].vars = vars;
  symbolstacks[stack_count].vars_cnt = vars_cnt;  
  stack_count++;
}

void pop_symbols(){
  stack_count--;
}

var_def * get_stack_variable(expr * name){
  for(i64 j = stack_count-1; j >= 0; j--){
    symbol_stack ss = symbolstacks[j];
    var_def * vars = *ss.vars;
    size_t varcnt = *ss.vars_cnt;
    for(size_t i = 0;i < varcnt; i++){
      if(name != vars[i].name){
	goto next_item;
      }
      return vars + i;
    next_item:
      continue;
    }
  }
  return NULL;
}

#include "uthash.h"

typedef struct {
  char * key;
  char * cname;
  UT_hash_handle hh;
}symbol_lookup;

// Converts from base radix A numbers to radix B.
void basea2b(bool (* read_glyph)(u8 * ctn), u32 from_base, u32 to_base, void (*emit_glyph)(u8 glyph)){
  
  double bitstack = 0;
  double basecount = 1;

  while(true){
    while(basecount < to_base){
      u8 chr;
      if(!read_glyph(&chr)) goto exit;
      bitstack = bitstack + basecount * chr;
      basecount *=  from_base;
    }

    emit_glyph((u32)bitstack % to_base);
    bitstack /= to_base;
    basecount /= to_base;
  }
 exit:
  if(basecount > 1)
    emit_glyph(bitstack);
}


char base61char(int i){
  if(i < 10)
    return '0' + i;
  if(i < 10 + 26)
    return 'A' + (i - 10);
  if(i < 10 + 26 + 26)
    return 'a' + (i - 10 - 26);
  if(i == 62) return '_';
  return '=';
}

int ibase61char(char c){
  if(c >= '0' && c <= '9')
    return c - '0';
  if(c >= 'A' && c<= 'Z')
    return c - 'A' + 10;
  if(c >= 'a' && c <= 'z')
    return c - 'a' + 10 + 26;
  if(c == '_')
    return 62; //'_'
  else return 63;
}

void base61format(char * str, char * output){
  bool next_glyph(u8 * chr){
    if(*str == 0) return false;
    *chr = *str;
    str++;
    return true;
  }
  void emit_glyph(u8 chr){
    *output = base61char(chr);
    output++;
    *output = 0;
  }
  basea2b(next_glyph, 256, 63, emit_glyph);
}

bool is_valid_c(char * str){
  if(isdigit(str[0])) return false;
  for(;*str != 0; str++){
    if(!(is_alphanum(*str) || *str =='_'))
      return false;
  }
  return true;
}

char cname[1000];
char * get_c_name(expr * s){
  if(s->type == VALUE && is_valid_c(s->value)){
    return s->value;
  }
  i64 v = interned_index(s);
  sprintf(cname, "S_%i", v);
  return cname;
}

char backbuffers[1000][3];
int backbuf = 0;
char * symbol_name(expr * e){
  int backbuffer = (backbuf++) % 3;
  char * out = expr_to_string(*e);
  strcpy(backbuffers[backbuffer], out);
  dealloc(out);
  return backbuffers[backbuffer];
}

void format_c_name(expr * s){
  format("%s", get_c_name(s));
}

#define EXPR_CHUNK_SIZE 1000
expr ** expr_chunks = NULL;
u64 * expr_taken = 0;
size_t expr_chunk_cnt = 0;

expr * get_interned(expr * e){
  for(size_t i = 0; i < expr_chunk_cnt; i++){
    if( e >= expr_chunks[i] && e < (expr_chunks[i] + expr_taken[i]))
      return e;
  }
  return NULL;
}

i64 interned_index(expr * e){
  for(size_t i = 0; i < expr_chunk_cnt; i++){
    if( e >= expr_chunks[i] && e < (expr_chunks[i] + EXPR_CHUNK_SIZE))
      return (i64)i * EXPR_CHUNK_SIZE + ((i64)(e - expr_chunks[i]));
  }
  return -1;
}

expr * alloc_interned(){
  if(expr_chunk_cnt == 0 || expr_taken[expr_chunk_cnt - 1] == EXPR_CHUNK_SIZE){
    void * nptr = alloc0(sizeof(expr) * EXPR_CHUNK_SIZE);
    list_add((void **) &expr_chunks, &expr_chunk_cnt, &nptr, sizeof(expr *));
    expr_chunk_cnt -= 1;
    size_t nv = 0;
    list_add((void **) &expr_taken, &expr_chunk_cnt, &nv, sizeof(size_t));
  }
  logd("alloc: %i\n", expr_chunk_cnt * EXPR_CHUNK_SIZE + expr_taken[expr_chunk_cnt - 1] + 1);
  return expr_chunks[expr_chunk_cnt - 1] + expr_taken[expr_chunk_cnt - 1]++;
}

expr * intern_expr(expr * e){
  // check if this expression is already interned. 
  expr * interned = get_interned(e);
  if(interned != NULL){
    logd("found!\n");
    return interned;
  }
  
  if(e->type == EXPR){
    
    expr * nexprs[e->sub_expr.cnt];
    for(size_t i = 0; i < e->sub_expr.cnt; i++){
      nexprs[i] = intern_expr(e->sub_expr.exprs[i]);
    }
    { // Find similar interned expression and return that
      for(size_t chunk_it = 0; chunk_it < expr_chunk_cnt; chunk_it++){
	u64 s = expr_taken[chunk_it];
	expr * chunk = expr_chunks[chunk_it];
	for(size_t i = 0; i < s; i++){
	  expr * e2 = chunk + i;
	  if(e2->type == EXPR && e2->sub_expr.cnt == e->sub_expr.cnt){
	    for(size_t j = 0; j< array_count(nexprs);j++){
	      if(e2->sub_expr.exprs[j] != nexprs[j]){
		goto next_item;
	      }
	    }
	    return e2;
	  }
	next_item:
	  continue;
	}
      }
    }
    { // Allocate new interned expression

      expr * out = alloc_interned();
      out->sub_expr.exprs = alloc0(sizeof(expr *) * e->sub_expr.cnt);
      for(size_t i = 0; i < e->sub_expr.cnt; i++)
	out->sub_expr.exprs[i] = nexprs[i];
      out->sub_expr.cnt = e->sub_expr.cnt;
      out->type = EXPR;
      return out;
    }
    
  }else if(e->type == VALUE){
    for(size_t chunk_it = 0; chunk_it < expr_chunk_cnt; chunk_it++){
	u64 size = expr_taken[chunk_it];
	expr * chunk = expr_chunks[chunk_it];
	for(size_t i = 0; i < size; i++){
	  expr * e2 = chunk + i;
	  if(e2->type == VALUE && strcmp(e2->value, e->value) == 0){

	    return e2;
	  }
	}
    }
    
    expr * out = alloc_interned();
    out->type = VALUE;
    out->value = clone(e->value,strlen(e->value) + 1);
      // Find similar interned symbol
    return out;
  }
  UNREACHABLE();
  return NULL;
}

bool test_intern_expr1(){
  expr * last = NULL;
  expr * a = alloc_interned();
  for(int i = 0; i < 10000; i++){
    last = a;
    a = alloc_interned();
    if(((int)(a - last)) != 1)
      logd("I: %i %i\n", i, (a - last));
  }
  logd("chunk cnt: %i\n", expr_chunk_cnt);
  return TEST_SUCCESS;
}

bool test_intern_expr(){
  expr e, ee, eee;
  lisp_parse(lisp_parse(lisp_parse("(hello a b c) (hello2) hello", &e), &ee), &eee);
  print_expr(&e);
  for(int i = 0; i < 100; i++){
    expr * e1 = intern_expr(&e);
    expr * e2 = intern_expr(&e);
    expr * e3 = intern_expr(&ee);
    expr * e4 = intern_expr(&ee);
    expr * e5 = intern_expr(&eee);
    TEST_ASSERT(e1 == e2);
    TEST_ASSERT(e1 != e3);
    TEST_ASSERT(e4 == e3);
    i64 n1 = interned_index(e1);
    i64 n2 = interned_index(e3);
    TEST_ASSERT(n1 != n2);
    i64 n3 = interned_index(e2);
    TEST_ASSERT(n1 == n3);
    i64 n4 = interned_index(e5);
    TEST_ASSERT(n4 == 0);
  }
  return TEST_SUCCESS;
}

bool test_get_cname(){
  char * get_c(char * str){return get_c_name(get_symbol(str));};
  char * n1 = get_c("thingy");
  char * n2 = get_c("2thingy");
  char * n3 = get_c("321");
  char * n4 = get_c("+");
  char * n5 = get_c("things-and-epic-things");
  char * n6 = get_c("+");
  char * n7 = get_c("+plus");
  char * n8 = get_c("+plus");
  
  logd("%s %s %s %s %s %s %s %s\n",n1, n2, n3, n4, n5, n6, n7, n8);
  TEST_ASSERT(n6 == n4);
  TEST_ASSERT(n7 == n8);
  return TEST_SUCCESS;
}

bool test_symbol_table(){
  expr * s = get_symbol("test");
  expr * s2 = get_symbol("__TEST__");
  expr * s3 = get_symbol("test");
  expr * s4 = get_symbol("__TEST__");
  UNUSED(s);UNUSED(s2);UNUSED(s3);UNUSED(s4);
  //TEST_ASSERT(symbol_cmp(s2, s4) && symbol_cmp(s, s3) && !symbol_cmp(s, s2));
  //TEST_ASSERT(strcmp(symbol_name(s3), "test") == 0);
  //TEST_ASSERT(strcmp(symbol_name(s4), "__TEST__") == 0);
  int scnt = 1000;
  expr ** symbols = alloc0(sizeof(expr *) * scnt);
  for(int i = 0; i < scnt; i++){
    char buf[20];
    sprintf(buf, "s%i", i);
    symbols[i] = get_symbol(buf);
  }					       
  i64 lastid = -1;
  for(int i = 0; i < scnt; i++){
    char buf[20];
    sprintf(buf, "s%i", i);
    TEST_ASSERT(interned_index(get_symbol(buf)) == interned_index(symbols[i]));
    TEST_ASSERT(interned_index(get_symbol(buf)) != lastid);
    lastid = interned_index(get_symbol(buf));
  }
  
  for(int i = 0 ; i < 63; i++){
    char mangled = base61char(i);
    int demangled = ibase61char(mangled);
    format("%i: %c %i\n", i, mangled, demangled);
    TEST_ASSERT(i == demangled);
  }

  char testbuf[100];
  char testinput[] = "e210hye291";
  base61format(testinput, testbuf);
  logd("%s\n",testbuf);

  return TEST_SUCCESS;
}

bool test_symbols(){
  TEST(test_get_cname);
  TEST(test_symbol_table);
  return TEST_SUCCESS;
}


