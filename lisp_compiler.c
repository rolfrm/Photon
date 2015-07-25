#include <stdio.h>
#include <iron/full.h>
#include "lisp_parser.h"
#include <libtcc.h>
#include "lisp_types.h"
#include "c_ast.h"
#include "lisp_compiler.h"
#include "lisp_std_types.h"
#include "type_pool.h"
#define VAR_BLOCK_SIZE 1000

struct _compiler_state{
  var_def ** vars; 
  size_t cnt;
  size_t used;
};

static __thread compiler_state * lisp_state = NULL;
static __thread compiler_state * lisp_states[10] = {NULL};
size_t state_count = 0;

void push_compiler(compiler_state * c){
  lisp_states[state_count] = lisp_state;
  lisp_state = c;
  state_count++;
}

void pop_compiler(){
  state_count--;
  lisp_state = lisp_states[state_count];
}

compiler_state * get_compiler(){
  return lisp_state;
}

var_def * get_global(symbol name){
  ASSERT(lisp_state != NULL);// sanity
  size_t varcnt = lisp_state->cnt;
  for(size_t i = 0;i < varcnt; i++){
    var_def * vars = lisp_state->vars[i];
    size_t offset = i * VAR_BLOCK_SIZE;
    size_t rest = lisp_state->used - offset;
    size_t lim = MIN(((size_t)VAR_BLOCK_SIZE), rest);
    for(size_t j = 0; j < lim; j++){
      if(symbol_cmp(name, vars[j].name))
	return vars + j;
    }
  }
  return NULL;
}

// check get_global first!
var_def * new_global(compiler_state * state){
  ASSERT(state != NULL);
  if(state->used + 1 > state->cnt * VAR_BLOCK_SIZE){
    var_def * newblock = alloc0(sizeof(var_def) * VAR_BLOCK_SIZE);
    list_add((void **) &state->vars, &state->cnt, &newblock, sizeof(var_def*));
  }
  size_t offset = state->used % VAR_BLOCK_SIZE;
  state->used++;
  return state->vars[state->cnt - 1] + offset;
}

void define_variable(symbol name, type_def * t, void * data, bool is_ptr){
  t = type_pool_get(t);
  var_def * var = get_global(name);
  if(var == NULL)
    var = new_global(lisp_state);
  var->name = name;
  var->type = t;
  var->data = data;
  var->is_ptr = is_ptr;
  ASSERT(get_global(name) != NULL); //sanity
 
}

void define_macro(char * name, int nargs, void * fcn){
  cmacro_def cast_def;
  cast_def.arg_cnt = nargs;
  cast_def.fcn = fcn;;
  cast_def.name = name;
  define_variable(get_symbol(name), &cmacro_def_def, clone(&cast_def,sizeof(cmacro_def)), false);
}

compiler_state * compiler_make(){
  return alloc0(sizeof(compiler_state));
}
