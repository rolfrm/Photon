#include <stdio.h>
#include <iron/full.h>
#include "lisp_parser.h"
#include <libtcc.h>
#include "lisp_types.h"
#include "lisp_std_types.h"
#include "lisp_compiler.h"
#include <readline/readline.h>
#include <readline/history.h>
#include <stdlib.h>
int check_expression(int status, char * buffer){
  while(*buffer){
    if(*buffer == '(')
      status += 1;
    if(*buffer == ')')
      status -= 1;
    if(*buffer == '\\')
      buffer++;
    buffer++;
  }
  return status;
}

bool start_read_eval_print_loop(compiler_state * c){
  log("PHOTON REPL\n");
  char * code = NULL;
  int expr_state = 0;
  while(true){
    while(code == NULL || 0 != (expr_state = check_expression(expr_state, code))){
    
      if(code == NULL){
	char * new_code = readline("> ");
	code = fmtstr("%s",new_code);
	free(new_code);
      }else{
	char * new_code = readline("> ");
	char * new2 = fmtstr("%s",new_code);
	free(new_code);
	char * new3 = fmtstr("%s%s", code, new2);
	dealloc(code);
	dealloc(new2);
	code = new3;
      }
    }

    if(expr_state < 0){
      ERROR("Invalid paren matching");
      goto reset;
    }
    
    size_t exprcnt;
    expr * exprs = lisp_parse_all(code, &exprcnt);
    lisp_run_exprs(c, exprs, exprcnt);
    
  reset:
    dealloc(code);
    code = NULL;
  } 
  return true;
}


void repl(){
  compiler_state * c = compiler_make();
  lisp_load_compiler(c);
  start_read_eval_print_loop(c);
}

