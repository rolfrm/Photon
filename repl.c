#include <stdio.h>
#include <iron/full.h>
#include "lisp_parser.h"
#include <libtcc.h>
#include "lisp_types.h"
#include "lisp_std_types.h"
#include "lisp_compiler.h"

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

  char * expr_reader = NULL;
  size_t cnt;
  FILE * mem = NULL;
  while(true){
    if(expr_reader == NULL){
      cnt = 0;
      mem = open_memstream(&expr_reader,&cnt);
      format(">");
    }else{
      format(" ");
    }

    char * data = NULL;
    size_t cnt2;
    int expr_state = 0;
    getline(&data,&cnt2,stdin);
    while(0 != (expr_state = check_expression(expr_state, data))){
      if(expr_state < 0){
	ERROR("Invalid paren matching");
	goto reset;
      }
      with_format_out(mem,lambda(void,(){format("%s",data);}));
      fflush(mem);
      getline(&data,&cnt2,stdin);
    }
    
    size_t exprcnt;
    expr * exprs = lisp_parse_all(data, &exprcnt);
    lisp_run_exprs(c, exprs, exprcnt);
    
  reset:
    fclose(mem);
    dealloc(expr_reader);
    expr_reader = NULL;
    cnt = 0;
  } 
  return true;
}


void repl(){
  compiler_state * c = compiler_make();
  lisp_load_compiler(c);
  start_read_eval_print_loop(c);
}

