#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdarg.h>
#include <iron/types.h>
#include <iron/log.h>
#include <iron/test.h>

#include "lisp_parser.h"
#include "lisp_types.h"
#include "c_ast.h"
#include "lisp_compiler.h"
#include "repl.h"

const char * allowed_errors[] ={
  "Unknown touch device",
  "Invalid renderer"
};

#include <signal.h>
bool faulty = false;
bool break_on_errors = true;
void _error(const char * file, int line, const char * str, ...){
  char buffer[1000];  
  va_list arglist;
  va_start (arglist, str);
  vsprintf(buffer,str,arglist);
  va_end(arglist);
  bool noncritical = false;
  for(u32 i = 0; i < array_count(allowed_errors);i++)
    if(strstr(buffer, allowed_errors[i]) != NULL)
      noncritical = true;
  if(!noncritical)
    faulty = true;
  
  loge("** ERROR at %s:%i **\n",file,line);
  if(noncritical && !break_on_errors) return; //skip noncritical errors
  loge("%s", buffer);
  printf("\n");
  printf("** **\n");
  if(break_on_errors)
    raise(SIGINT);
}

bool lisp_compiler_test();
bool test_lisp_parser();
bool test_lisp2c();
bool test_get_cname();
bool test_symbols();
bool test_type_pool();
int main(int argc, char *argv[] ){
  if(argc == 1 || (argc >= 2 && strcmp(argv[1],"--repl") == 0)){
    break_on_errors = false;
    repl(argc > 2 ? argv[2] : NULL);
    return 0;
  }
  
  if(argc == 2 && strcmp(argv[1],"--test") == 0){
    log("Running tests...\n");
    compiler_state * c = compiler_make();
    push_compiler(c);
    TEST(test_symbols);
    TEST(test_lisp_parser);
    TEST(test_type_pool);
    TEST(test_lisp2c);

    pop_compiler();

    return 0;
  }

  if(argc == 2){
    char * filename = argv[1];
    if(access(filename, F_OK) == -1){
      loge("Error: File '%s' does not exist\n", filename);
      return 1;
    }
    compiler_state * c = compiler_make();
    push_compiler(c);
    lisp_load_base();


    compile_status status = lisp_run_script_file(argv[1]);
    if(status == COMPILE_ERROR){
      return -1;
    }
    pop_compiler();
    return 0;
  }

  return 0;
}
