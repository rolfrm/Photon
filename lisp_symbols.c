#include <stdio.h>
#include <iron/full.h>
#include "lisp_parser.h"
#include <libtcc.h>
#include "lisp_types.h"
#include "lisp_compiler.h"
#include "lisp_std_types.h"

//todo: Optimize to use symbols instead.

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

/*type_def * name_to_type_def(char * name, size_t len){
  if(strncmp("void",name,len) == 0){
    return &void_def;
  }
  compiler_state * c = lisp_state;
  for(size_t i = 0;i < c->var_cnt; i++){
    var_def * v = c->vars + i;
    logd("loc type\n");
    if(v->type ! = type_def_def)
      goto next_item;
    type_def * d = (type_def *) v->data;
    char * tname = NULL;
    if(d->kind == SIMPLE){
      tname = d->simple.name;
    }
    logd("TYPE: %s\n",tname);
    if(tname != NULL){
      if(strncmp(tname,name,len) ==0){
	return d;
      }
    }
  next_item:
    continue;
  }
  return NULL;
  }*/


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
