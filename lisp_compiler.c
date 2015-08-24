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
  char * compile_out_file;
};

compiler_state * lisp_current_compiler = NULL;

compiler_state * lisp_make_compiler(){
  return alloc0(sizeof(compiler_state));
}

void set_compile_out(compiler_state * compiler, char * path){
  compiler->compile_out_file = path;
}

char * get_compile_out(compiler_state * compiler){
  return compiler->compile_out_file;
}

var_def * get_global(symbol name){
  ASSERT(lisp_current_compiler != NULL);// sanity
  size_t varcnt = lisp_current_compiler->cnt;
  for(size_t i = 0;i < varcnt; i++){
    var_def * vars = lisp_current_compiler->vars[i];
    size_t offset = i * VAR_BLOCK_SIZE;
    size_t rest = lisp_current_compiler->used - offset;
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
    var = new_global(lisp_current_compiler);
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
  define_variable(get_symbol(name), &cmacro_def_def, clone(&cast_def,sizeof(cmacro_def)), false);
}

compiler_state * compiler_make(){
  return alloc0(sizeof(compiler_state));
}
