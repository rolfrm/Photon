#include <stdio.h>
#include <iron/full.h>
#include "lisp_parser.h"
#include <libtcc.h>
#include "lisp_types.h"
#include "lisp_compiler.h"
#include "lisp_std_types.h"
#include <ctype.h>

#include "uthash.h"

typedef struct{
  char * key;
  u64 value;
  UT_hash_handle hh;
  UT_hash_handle hh2;
}symbol_table;
const symbol symbol_empty = {0};
symbol_table * symtbl = NULL; 
symbol_table * symtbl_byid = NULL; 
u32 symbol_cnt = 1;

symbol * get_symbol2(char * name){
  symbol_table * sym_item = NULL;
  HASH_FIND_STR(symtbl, name, sym_item);
  if(sym_item == NULL){
    
    sym_item = alloc0(sizeof(symbol_table));
    sym_item->value = ++symbol_cnt;
    sym_item->key = fmtstr("%s", name);
    HASH_ADD_KEYPTR(hh, symtbl, sym_item->key, strlen(sym_item->key), sym_item);
    HASH_ADD(hh2, symtbl_byid, value, sizeof(sym_item->value), sym_item);
  }
  return (symbol *) &sym_item->value;
}

symbol get_symbol(char * name){
  return *get_symbol2(name);
}

char * symbol_name(symbol s){
  symbol_table * sym_item = NULL;
  HASH_FIND(hh2, symtbl_byid, &s.id,sizeof(s.id), sym_item);
  if(sym_item == NULL)
    return NULL;
  return sym_item->key;
}
bool symbol_cmp(symbol a, symbol b){
  return (a.id == b.id);
}

typedef struct _symbol_stack symbol_stack;
struct _symbol_stack{
  var_def ** vars;
  size_t *vars_cnt;
  symbol_stack * tail;
};
__thread symbol_stack * symbolstack = NULL;

void with_symbols(var_def ** vars, size_t * vars_cnt, void (*fcn)()){
  symbol_stack nss;
  nss.vars = vars;
  nss.vars_cnt = vars_cnt;
  nss.tail = symbolstack;
  symbol_stack * oss = symbolstack;
  symbolstack = &nss;
  fcn();
  symbolstack = oss;
}

var_def * get_variable(symbol name){
  symbol_stack * ss = symbolstack;
  while(ss != NULL){
    var_def * vars = *ss->vars;
    size_t varcnt = *ss->vars_cnt;
    for(size_t i = 0;i < varcnt; i++){
      if(!symbol_cmp(name,vars[i].name)){
	goto next_item;
      }
      return vars + i;
    next_item:
      continue;
    }
    ss = ss->tail;
  }
  return NULL;
}

#include "uthash.h"

typedef struct {
  char * key;
  char * cname;
  UT_hash_handle hh;
}symbol_lookup;

char * get_c_name(symbol s){
  char * sym = symbol_name(s);
  bool first_alpha = isalpha(sym[0]) || '_' == sym[0];
  bool all_alphanum = true;
  for(size_t i = 0; sym[i];i++){
    if(isalnum(sym[i]) == false && sym[i] !='_'){
      all_alphanum = false;
      break;
    }
  }
  
  if(all_alphanum){
    if(first_alpha) return sym;
    return fmtstr("S_%s",sym);
  }else{
    static i32 uniqueid = 0;
    static symbol_lookup * lut = NULL;
    symbol_lookup _lut_item;
    symbol_lookup * lut_item = &_lut_item;
    HASH_FIND_STR(lut, sym, lut_item);
    if(lut_item != NULL) return lut_item->cname;
    lut_item = alloc0(sizeof(symbol_lookup));
    char * value = fmtstr("S__%i", uniqueid);

    lut_item->key = sym;
    lut_item->cname = value;
    HASH_ADD_KEYPTR(hh, lut, lut_item->key, strlen(lut_item->key), lut_item);
    uniqueid += 1;
    return value;
  }
}

cmacro_def * get_cmacro_def(symbol s){
  var_def * var = get_variable(s);
  if(var == NULL){
    return NULL;
  }
  
  if(var->type != &cmacro_def_def){
    return NULL;
  }
  return (cmacro_def *) var->data;
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
  symbol s = get_symbol("test");
  symbol s2 = get_symbol("__TEST__");
  symbol s3 = get_symbol("test");
  symbol s4 = get_symbol("__TEST__");
  TEST_ASSERT(symbol_cmp(s2, s4) && symbol_cmp(s, s3) && !symbol_cmp(s, s2));
  TEST_ASSERT(strcmp(symbol_name(s3), "test") == 0);
  TEST_ASSERT(strcmp(symbol_name(s4), "__TEST__") == 0);
  return TEST_SUCCESS;
}

bool test_symbols(){
  TEST(test_get_cname);
  TEST(test_symbol_table);
  return TEST_SUCCESS;
}


