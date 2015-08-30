#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdarg.h>
#include <iron/types.h>
#include <iron/log.h>
#include <iron/test.h>
#include <iron/mem.h>
#include "lisp_parser.h"
#include "lisp_types.h"
#include "c_ast.h"
#include "lisp_compiler.h"

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

#ifdef _WIN32
#undef ASSERT
#include <windows.h>
char * get_exe_path(){

  char pBuf[100];
  int bytes = GetModuleFileName(NULL, pBuf, array_count(pBuf)));
  ASSERT(bytes > 0);
 return fmtstr("%s",pBuf);
#else
char * get_exe_path(){
  char szTmp[32];
  char pBuf[100];
  memset(pBuf,0,sizeof(pBuf));
  sprintf(szTmp, "/proc/%d/exe", getpid());
  int bytes = readlink(szTmp, pBuf, 100);
  ASSERT(bytes > 0);
  return fmtstr("%s", pBuf);
#endif
}

static bool run_test = false;
static char * c_compile_out = NULL;
static char * file_to_run = NULL;
static char * exec_path = NULL;

void parse_args(char * argv[], int cnt){
  exec_path = get_exe_path();
  for(int i = 1; i < cnt; i++){
    char * arg = argv[i];
    bool is_test = strcmp(arg,"--test") == 0;
    if(is_test){
      run_test = true;
      continue;
    }
    bool compile_out = strcmp(arg,"--compile-out") == 0;
    if(compile_out){
      i++;
      c_compile_out = fmtstr("%s",argv[i]);
      continue;
    }
    file_to_run = argv[i];
  }
}
int main(int argc, char *argv[] ){

  parse_args(argv, argc);
  logd("Running from %s\n", exec_path);
  if(run_test){
    log("Running tests...\n");
    lisp_current_compiler = lisp_make_compiler();
    set_compile_out(lisp_current_compiler, c_compile_out);
    TEST(test_symbols);
    //TEST(test_lisp_parser);
    TEST(test_type_pool);
    TEST(test_lisp2c);
    return 0;
  }

  if(file_to_run != NULL){
    char * filename = argv[1];
    if(access(filename, F_OK) == -1){
      loge("Error: File '%s' does not exist\n", filename);
      return 1;
    }
    lisp_current_compiler = lisp_make_compiler();
    set_compile_out(lisp_current_compiler, c_compile_out);
    lisp_load_base(exec_path);
    define_variable(get_symbol("break-on-errors"),str2type("bool"),&break_on_errors, true);
    compile_status status = lisp_run_script_file(argv[1]);
    if(status == COMPILE_ERROR){
      return -1;
    }
    return 0;
  }
  
  log("Unable to handle arguments: ");
  for(int i = 1; i < argc; i++){
    log("%s ", argv[i]);
  }
  log("\n");
  logd("Add the name of an existing file like this: './foton growth.lisp'\n");

  return 0;
}
