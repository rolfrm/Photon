#include <stdio.h>
#include <iron/full.h>
#include "lisp_parser.h"
#include <libtcc.h>
#include "lisp_types.h"
#include "c_ast.h"
#include "lisp_compiler.h"
#include "lisp_std_types.h"
#include "type_pool.h"

struct _compiler_state{
  var_def * vars;//[100];
  size_t var_cnt;
};

static __thread compiler_state * lisp_state = NULL;
static __thread compiler_state * lisp_states[10] = {NULL};
size_t state_count = 0;

void push_compiler(compiler_state * c){
  lisp_states[state_count] = lisp_state;
  lisp_state = c;
  state_count++;
  //push_symbols(&c->vars,&c->var_cnt);
}

void pop_compiler(){
  state_count--;
  lisp_state = lisp_states[state_count];
  //pop_symbols();
}

compiler_state * get_compiler(){
  return lisp_state;
}

var_def * get_global(symbol name){
  ASSERT(lisp_state != NULL);// sanity
  var_def * vars = lisp_state->vars;
  size_t varcnt = lisp_state->var_cnt;
  for(size_t i = 0;i < varcnt; i++){
    if(!symbol_cmp(name,vars[i].name)){
      goto next_item;
    }
    return vars + i;
  next_item:
    continue;
  }
  return NULL;
}


void compiler_define_variable_ptr(symbol name, type_def * t, void * ptr){
  // check if reassign can be done.
  t = type_pool_get(t);
  var_def * var = get_global(name);
  if(var != NULL){
    var->type = t;
    var->data = ptr;
    return;
  }
  
  var_def vdef;
  vdef.name = name;
  vdef.type = t;
  vdef.data = ptr;
  list_add((void **)&lisp_state->vars, &lisp_state->var_cnt, &vdef, sizeof(var_def));
}

void define_variable(symbol name, type_def * t, void * data){
  t = type_pool_get(t);
  var_def * var = get_global(name);
  if(var == NULL){
    var_def vdef;
    vdef.name = name;
    vdef.type = t;
    vdef.data = alloc0(sizeof(void *));
    list_add((void **)&lisp_state->vars, &lisp_state->var_cnt, &vdef, sizeof(var_def));
    var = lisp_state->vars + (lisp_state->var_cnt - 1);
  }
  ASSERT(get_global(name) != NULL); //sanity
  if(var != NULL)
    *((void **)var->data) = data;
  
}

void define_macro(char * name, int nargs, void * fcn){
  cmacro_def cast_def;
  cast_def.arg_cnt = nargs;
  cast_def.fcn = fcn;;
  cast_def.name = name;
  compiler_define_variable_ptr(get_symbol(name), &cmacro_def_def, clone(&cast_def,sizeof(cmacro_def)));
}

compiler_state * compiler_make(){
  return alloc0(sizeof(compiler_state));
}
