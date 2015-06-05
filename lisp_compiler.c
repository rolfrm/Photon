#include <stdio.h>
#include <iron/full.h>
#include "lisp_parser.h"
#include <libtcc.h>
#include "lisp_types.h"
#include "lisp_compiler.h"
#include "lisp_std_types.h"

//static __thread comp_state * compstate = NULL;
static __thread compiler_state * lisp_state = NULL;

void with_compiler(compiler_state * c, void (* fcn)()){
  compiler_state * old = lisp_state;
  lisp_state = c;
  with_symbols(&c->vars,&c->var_cnt,fcn);
  lisp_state = old;
}

compiler_state * get_compiler(){
  return lisp_state;
}


void compiler_define_variable_ptr(symbol name, type_def * t, void * ptr){
  // check if reassign can be done.

  for(size_t i = 0; i < lisp_state->var_cnt; i++){
    if(symbol_cmp(name, lisp_state->vars[i].name)){

	lisp_state->vars[i].type = t;
	lisp_state->vars[i].data = ptr;
	return;
    }
  }
  
  var_def vdef;
  vdef.name = name;
  vdef.type = t;
  vdef.data = ptr;
  list_add((void **)&lisp_state->vars, &lisp_state->var_cnt, &vdef, sizeof(var_def));
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
