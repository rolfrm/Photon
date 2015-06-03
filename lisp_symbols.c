#include <stdio.h>
#include <iron/full.h>
#include "lisp_parser.h"
#include <libtcc.h>
#include "lisp_types.h"
#include "lisp_compiler.h"
#include "lisp_std_types.h"
#include <ctype.h>

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

var_def * get_variable(char * name, size_t name_len){
  if(name_len > 1024)
    ERROR("Excessive variable name length");
  symbol_stack * ss = symbolstack;
  while(ss != NULL){
    var_def * vars = *ss->vars;
    size_t varcnt = *ss->vars_cnt;
    for(size_t i = 0;i < varcnt; i++){
      for(size_t j = 0; j < name_len; j++){
	if(name[j] != vars[i].name[j])
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

var_def * get_variable2(char * name){
  return get_variable(name,strlen(name));
}

var_def * get_function(char * name, type_def * type){
  size_t name_len = strlen(name);
  symbol_stack * ss = symbolstack;
  while(ss != NULL){
    var_def * vars = *ss->vars;
    size_t varcnt = *ss->vars_cnt;
    for(size_t i = 0;i < varcnt; i++){
      for(size_t j = 0; j < name_len; j++){
	if(name[j] != vars[i].name[j])
	  goto next_item;
      }
      if(vars[i].type == type)
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

char * get_c_name(char * sym){
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

bool test_get_cname(){
  char * n1 = get_c_name("thingy");
  char * n2 = get_c_name("2thingy");
  char * n3 = get_c_name("321");
  char * n4 = get_c_name("+");
  char * n5 = get_c_name("things-and-epic-things");
  char * n6 = get_c_name("+");
  char * n7 = get_c_name("+plus");
  char * n8 = get_c_name("+plus");
  
  logd("%s %s %s %s %s %s %s %s\n",n1, n2, n3, n4, n5, n6, n7, n8);
  TEST_ASSERT(n6 == n4);
  TEST_ASSERT(n7 == n8);
  return TEST_SUCCESS;
}

fcn_def * get_fcn_def(char * name, size_t name_len){
  var_def * var = get_variable(name, name_len);
  if(var == NULL){
    return NULL;
  }
  
  if(var->type != &fcn_def_def){
    return NULL;
  }
  return (fcn_def *) var->data;
}

cmacro_def * get_cmacro_def(char * name, size_t name_len){
  var_def * var = get_variable(name, name_len);
  if(var == NULL){
    return NULL;
  }
  
  if(var->type != &cmacro_def_def){
    return NULL;
  }
  return (cmacro_def *) var->data;
}
