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
    if(symbol_cmp(name,lisp_state->vars[i].name)){
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
/*
//move to iron
bool seek_test(){
  char * expr_reader = NULL;
  size_t cnt;
  FILE * mem = NULL;
  mem = open_memstream(&expr_reader,&cnt);
  with_format_out(mem,lambda(void,
			     (){
			       format("hello\nhello2\nhello3");
			       fflush(mem);
			       seek_back_newline(mem);
			       fflush(mem);
			       format("hello4!");
			       seek_back_newline(mem);
			       seek_back_newline(mem);
			       format("hello5?");
			     }));

  fclose(mem);
  if(strcmp("hello\nhello2\nhello5?",expr_reader) != 0)
    return TEST_FAIL;
  dealloc(expr_reader);
  return TEST_SUCCESS;
}

bool lisp_compiler_test(){
  load_defs();
  TEST(test_print_c_code);
  TEST(seek_test);
  { // testing var stack
  var_def vars1[] = {{1,&void_ptr_def,0},  {2,&void_ptr_def,0}};
  var_def vars2[] = {{4,&void_ptr_def,0}, {3,&void_ptr_def,0}, {4,&void_ptr_def,0}};
  int stacksize = 0;
  
  void a2(){
    symbol_stack * stk = symbolstack;
    while(stk != NULL){
      stacksize += 1;
      logd("Symbol stack: %i %i\n",stk->vars, stk->vars_cnt);
      for(size_t i = 0 ; i < stk->vars_cnt;i++){
	logd(" %i",stk->vars[i].name);
      }
      logd("\n");
      stk = stk->tail;
    }
  }
  
  void a1(){
    with_symbols(vars2,array_count(vars2),a2);
  }
  logd("Stacksize; %i\n", stacksize);

  with_symbols(vars1,array_count(vars1),a1);
  TEST_ASSERT(stacksize == 2);
  TEST_ASSERT(symbolstack == NULL);
  }
  compiler_state * c = compiler_make();
  compiler_set_state(c);
  print_def(void_def,0,false);
	  
  decl dcl;
  dcl.name = "test";
  dcl.type = void_ptr_def;
  char * testd = alloc0(10000);
  print_cdecl(dcl);
  load_cdecl(testd, 10000, dcl);
  type_def defs[1000];
  for(size_t i = 0; i < array_count(defs);i++)
    defs[i] = void_def;
  make_dependency_graph(defs,&type_def_def);
  make_dependency_graph(defs,&decl_def);

  { // print_string definition
    static type_def print_string_def;
    static decl args[1];
    args[0].name = "string";
    args[0].type = char_ptr_def;    
    print_string_def.kind = FUNCTION;
    print_string_def.fcn.ret = &void_def;
    print_string_def.fcn.cnt = array_count(args);
    print_string_def.fcn.args = args;
    fcn_def fdef = defext("print_string", print_string_def);
    fdef.is_extern = false;
    fdef.ptr = &print_string;
    fcn_def * var = (fcn_def *) compiler_define_variable(c, "print_string", fcn_def_def);
    format("print string: %i\n", var);
    TEST_ASSERT(var != NULL);
    *var = fdef;
  }

  { // define the function to define external functions.
    static type_def defext_def;
    static decl args[2];
    args[0].name = "name";
    args[0].type = char_ptr_def;
    args[1].name = "type";
    args[1].type = type_def_def;
    defext_def.kind = FUNCTION;
    defext_def.fcn.ret = &void_def;
    defext_def.fcn.cnt = array_count(args);
    defext_def.fcn.args = args;
    
    fcn_def fdef = defext("defext",defext_def);
    fcn_def * var = (fcn_def *) compiler_define_variable(c, "defext", fcn_def_def);
    format("defext: %i\n", var);
    *var = fdef;
  }

  {// the cast macro
    cmacro_def * var = (cmacro_def *) compiler_define_variable(c, "cast", cmacro_def_def);
    static cmacro_def cast_def;
    cast_def.arg_cnt = 2;
    cast_def.fcn = &cast_macro;
    cast_def.name = "cast";
    *var = cast_def;
  }

  {// the lol macro
    type_def lol(expr exp){
      logd("LOL: Compiling stuff..\n");
      return compile_iexpr(exp);
    }
    cmacro_def * var = (cmacro_def *) compiler_define_variable(c, "lol", cmacro_def_def);
    static cmacro_def cast_def;
    cast_def.arg_cnt = 1;
    cast_def.fcn = &lol;
    cast_def.name = "lol";
    *var = cast_def;
  }

  {//type_def type_macro(expr typexpr)
    cmacro_def * var = (cmacro_def *) compiler_define_variable(c, "type", cmacro_def_def);
    static cmacro_def cast_def;
    cast_def.arg_cnt = 1;
    cast_def.fcn = &type_macro;
    cast_def.name = "type";
    *var = cast_def;
  }
  {//type_def new_macro(expr typexpr)
    cmacro_def * var = (cmacro_def *) compiler_define_variable(c, "new", cmacro_def_def);
    static cmacro_def cast_def;
    cast_def.arg_cnt = 1;
    cast_def.fcn = &new_macro;
    cast_def.name = "new";
    *var = cast_def;
  }	  

  {//type_def split_macro(expr typexpr)
    cmacro_def * var = (cmacro_def *) compiler_define_variable(c, "split", cmacro_def_def);
    static cmacro_def cast_def;
    cast_def.arg_cnt = 1;
    cast_def.fcn = &split_macro;
    cast_def.name = "split";
    *var = cast_def;
  }	  

	  
  {//type_def defun_macro(expr name, expr typexpr, expr body)
    cmacro_def * var = (cmacro_def *) compiler_define_variable(c, "defun", cmacro_def_def);
    static cmacro_def cast_def;
    cast_def.arg_cnt = 3;
    cast_def.fcn = &defun_macro;
    cast_def.name = "defun";
    *var = cast_def;
  }
  
  {
    static type_def voidstr_def;
    voidstr_def.kind = FUNCTION;
    voidstr_def.fcn.cnt = 0;
    voidstr_def.fcn.ret = &char_ptr_def;
    voidstr_def.fcn.args = NULL;
    fcn_def fdef = defext("glfwGetVersionString",voidstr_def);
    
    fcn_def * var = (fcn_def *) compiler_define_variable(c, "glfwGetVersionString", fcn_def_def);
    *var = fdef;
  }
  {
    static type_def voidstr_def;
    voidstr_def.kind = FUNCTION;
    voidstr_def.fcn.cnt = 0;
    voidstr_def.fcn.ret = &void_def;
    voidstr_def.fcn.args = NULL;
    fcn_def fdef = defext("glfwInit",voidstr_def);
    
    fcn_def * var = (fcn_def *) compiler_define_variable(c, "glfwInit", fcn_def_def);
    *var = fdef;
  }

  int * list = NULL;
  size_t list_cnt = 0;
  for(int i = 0; i < 5;i++)
    list_add((void **)&list,&list_cnt,&i, sizeof(int));
  TEST_ASSERT(list_cnt == 5);
  TEST_ASSERT(list[4] = 4);
  TEST(tccs_test2);
  type_def * var = (type_def *) compiler_define_variable(c, "type_def_def", type_def_def);
  type_def * var2 = (type_def *) compiler_define_variable(c, "decl_def", decl_def);
  *((type_def *) var) = type_def_def;
  *((type_def *) var2) = decl_def;
  type_def * var3 = (type_def *) compiler_define_variable(c, "char_ptr_def", type_def_def);
  *var3 = char_ptr_def;
  type_def * i64var = (type_def *) compiler_define_variable(c, "i64_def", type_def_def);
  *i64var = i64_def;
  char * test_code = "(print_string \"Hello World\\n\")(glfwInit)(print_string (glfwGetVersionString)) (print_string \"\\nhello sailor!\\n\") (lol (glfwGetVersionString)) (cast 100 i64_def)";
  test_code = "(cast 10 i64_def)";
  test_code = "(new (fcn i64 (a i64)))";
  test_code = "(defun printhello ()(print_string \"hello\\n\"))";
  //test_code = "(split \"wtf\")";
  expr out_expr[2];
  char * next = test_code;
  while(next != NULL && *next != 0){
    int out = 2;
    char * prev = next;
    next = lisp_parse(next, out_expr, &out);
    TEST_ASSERT(prev != next);
    for(int i = 0; i < out; i++){
      compiled_expr expr = compile_expr(out_expr + i);
      void eval_print(compiled_expr expr);
      eval_print(expr);
      TEST_ASSERT(NULL != expr.fcn);
      delete_expr(out_expr + i);
    }
    if(out == 0)
      break;
  }
  bool start_read_eval_print_loop();
  //return start_read_eval_print_loop();
  return true;
}

int check_expression(char * buffer){
  int status = 0;
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

void eval_print(compiled_expr cexpr){
  if(NULL == cexpr.fcn){
    ERROR("Unable to compile..\n");
  }else if(type_def_cmp(cexpr.result_type, char_ptr_def)){
    char * (*eval) () = cexpr.fcn;
    logd("'%s' : char*\n", eval());
  }
  else if(type_def_cmp(cexpr.result_type, void_def)){
    void (* __eval) () = cexpr.fcn;
    __eval();
    logd("() : unit\n");
  }
  else if(type_def_cmp(cexpr.result_type, i64_def)){
    i64 (*eval) () = cexpr.fcn;
    eval();
    logd("%i : i64\n",eval());
  }else if(type_def_cmp(cexpr.result_type, type_def_def)){

    type_def (*eval) () = cexpr.fcn;
    type_def d = eval();
    UNUSED(d);
    //print_def(d,0,false);
    logd(" : type_def\n");
  }
}

bool start_read_eval_print_loop(){
  format("C-LISP REPL\n");

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
    getline(&data,&cnt2,stdin);
    with_format_out(mem,lambda(void,(){format("%s",data);}));
    fflush(mem);
    int expr_state = check_expression(expr_reader);
    if(expr_state < 0){
      ERROR("Invalid paren matching");
      goto reset;
    }
    if(expr_state > 0)
      continue;
    logd("Expr state: %i %s\n", expr_state, expr_reader);
    char * next = expr_reader;
    while(next != NULL && *next != 0){
      expr out_expr[2];
      int out = array_count(out_expr);
      char * prev = next;
      next = lisp_parse(next, out_expr, &out);
      if(prev == next){
	ERROR("Unable to parse..\n");
	continue;
      }
      for(int i = 0; i < out; i++){
	compiled_expr cexpr = compile_expr(out_expr + i);
	eval_print(cexpr);
	delete_expr(out_expr + i);
      }
      if(out == 0)
	break;
    }
  reset:
    fclose(mem);
    dealloc(expr_reader);
    expr_reader = NULL;
    cnt = 0;
  } 
  return true;
  }

void * tccs_compile_and_get(TCCState * tccs, char * code, char * symbol){
  format("Compiling %s\n", code);
  int fail = tcc_compile_string(tccs,code);
  format("COMPILE: %i\n",!fail);
  int size = tcc_relocate(tccs, NULL);
  char * codebuf = alloc(size);
  fail = tcc_relocate(tccs, codebuf);
  format("RELOCATE: %i\n",!fail);
  return tcc_get_symbol(tccs, symbol);
}

bool tccs_test2(){
  char * a = "int calc_x(){ return 5;}";
  char * b = "int calc_x(); int calc_y(){{{int v2 = 3; { int v = 7; return calc_x() + v + v2;}}}}";
  char * c = "int cval = 20;";
  TCCState * tccs = mktccs();

  int (* fcn)() =  tccs_compile_and_get(tccs, a, "calc_x");
  TEST_ASSERT(fcn() == 5);
  format("outp: %f\n", fcn());
  
  tcc_delete(tccs);
  tccs = mktccs();
  tcc_add_symbol(tccs, "calc_x", fcn);
  int (* fcn2)() = tccs_compile_and_get(tccs, b, "calc_y");
  TEST_ASSERT(fcn2() == 15);
  format("outp: %i\n", fcn2());
  tcc_delete(tccs);

  tccs = mktccs();
  int * var = tccs_compile_and_get(tccs, c, "cval");
  format("outp: %i\n", var);
  TEST_ASSERT(*var == 20);
  tcc_delete(tccs);
  return TEST_SUCCESS;
}
*/
