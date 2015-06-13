#include <stdio.h>
#include <iron/full.h>
#include "lisp_parser.h"
#include <libtcc.h>
#include "lisp_types.h"
#include "lisp_std_types.h"
#include "lisp_compiler.h"
#include "type_pool.h"
#include "builtin_macros.h"
#include "expr_utils.h"
type_def * type_macro(c_block * block, c_value * value, expr e){
  UNUSED(block);
  static int typevarid = 0;
  type_def * t =_type_macro(e);

  char buf[10];
  sprintf(buf, "type_%i",typevarid++);
  symbol varname = get_symbol(buf);
  compiler_define_variable_ptr(varname, &type_def_ptr_def, clone(&t,sizeof(type_def *)));
  value->type = C_INLINE_VALUE;
  value->raw.value = "NULL";
  value->raw.type = &type_def_ptr_def;
  type_def * rt = _compile_expr(block, value, symbol_expr2(varname));
  return rt;
}

expr mk_sub_expr(expr * exprs, size_t cnt){
  expr e;
  e.type = EXPR;
  e.sub_expr.exprs = exprs;
  e.sub_expr.cnt = cnt;
  return e;
}

type_def * var_macro(c_block * block, c_value * val, expr vars, expr body){
  COMPILE_ASSERT(vars.type == EXPR);
  sub_expr sexpr = vars.sub_expr;
  c_expr cvars[sexpr.cnt];
  c_value * cvals = alloc0(sizeof(c_value) * sexpr.cnt);
  var_def * lisp_vars = alloc(sizeof(var_def) * sexpr.cnt);
  for(size_t i = 0; i < sexpr.cnt; i++){
    COMPILE_ASSERT(sexpr.exprs[i].type == EXPR);
    sub_expr var_expr = sexpr.exprs[i].sub_expr;
    COMPILE_ASSERT(var_expr.cnt == 2 && var_expr.exprs[0].type == VALUE && var_expr.exprs[0].value.type == SYMBOL);
    c_var var;
    var.var.name = expr_symbol(var_expr.exprs[0]);
    var.var.type = _compile_expr(block, cvals + i, var_expr.exprs[1]);
    var.value = cvals + i;
    lisp_vars[i].name = var.var.name;
    lisp_vars[i].type = var.var.type;
    lisp_vars[i].data = NULL;
    cvars[i].type = C_VAR;
    cvars[i].var = var;
  }
  for(size_t i = 0; i < sexpr.cnt; i++){
    list_add((void **) &block->exprs, &block->expr_cnt, cvars + i, sizeof(c_expr));
  }
  
  type_def * ret_type;
  with_symbols(&lisp_vars,&sexpr.cnt,lambda(void,(){
	ret_type = _compile_expr(block,val,body);
      }));
  return ret_type;
}

type_def * defvar_macro(c_block * block, c_value * val, expr * exprs, size_t cnt){
  COMPILE_ASSERT(cnt == 2 || cnt == 3);
  expr name = exprs[0];
  COMPILE_ASSERT(is_symbol(name));
  symbol sym = expr_symbol(name);
  type_def * t;
 
  if(is_keyword(exprs[1]) && expr_symbol(exprs[1]).id == get_symbol("type").id && cnt == 3){
    
    t = _type_macro(exprs[2]);
    val->type = C_SYMBOL;
    val->symbol = sym;
  }else{
    COMPILE_ASSERT(cnt == 2);
    expr body = exprs[1];
    c_value * vr = alloc0(sizeof(c_value));
    c_value * vl = alloc0(sizeof(c_value));
    vl->type = C_SYMBOL;
    vl->symbol = sym;
    t = _compile_expr(block, vr, body);
    val->type = C_OPERATOR;
    val->operator.left = vl;
    val->operator.right = vr;
    val->operator.operator = '=';
  }

  c_root_code var_root;
  var_root.type = C_VAR_DEF;
  var_root.var.var.name = sym;
  var_root.var.var.type = t;
  var_root.var.value = NULL;
  compile_as_c(&var_root,1);
  return t;
}

type_def * setf_macro(c_block * block, c_value * val, expr name, expr body){
  COMPILE_ASSERT(is_symbol(name));
  symbol sym = expr_symbol(name);

  c_value * vr = alloc0(sizeof(c_value));
  c_value * vl = alloc0(sizeof(c_value));
  vl->type = C_SYMBOL;
  vl->symbol = sym;
  type_def * t = _compile_expr(block, vr, body);
  val->type = C_OPERATOR;
  val->operator.left = vl;
  val->operator.right = vr;
  val->operator.operator = '=';
  return t;
}

type_def * load_macro(c_block * block, c_value * val, expr file_name){
  COMPILE_ASSERT(is_string(file_name));
  char * filename = read_symbol(file_name);
  lisp_run_script_file(filename);
  return _compile_expr(block, val, file_name);
}

type_def * progn_macro(c_block * block, c_value * val, expr * expressions, size_t expr_cnt){
  // todo: requires varadic macros.
  type_def * d;
  for(size_t i = 0; i < expr_cnt; i++){
    c_value _val;
    d = _compile_expr(block, &_val, expressions[i]);
    if(i == expr_cnt -1){
      *val = _val;
      return d;
    }
    c_expr expr;
    expr.type = C_VALUE;
    expr.value = _val;
    list_add((void **) &block->exprs, &block->expr_cnt,&expr,sizeof(c_expr));
  }
  return &void_def;
}

type_def * opaque_expr(){
  static type_def * exprtd = NULL;
  if(exprtd == NULL){
    exprtd = str2type("(ptr (alias (opaque-struct _expr) expr))");
  }
  return exprtd;
}

type_def * expand_macro(c_block * block, c_value * val, expr * exprs, size_t cnt){
  COMPILE_ASSERT(cnt > 0);
  COMPILE_ASSERT(is_symbol(exprs[0]));
  
  type_def * exprtd = opaque_expr();

  symbol name = expr_symbol(exprs[0]);
  var_def * fcn_var = get_variable(name);
  COMPILE_ASSERT(fcn_var != NULL);
  type_def * fcn_type = fcn_var->type;
  COMPILE_ASSERT(fcn_type->type == FUNCTION && fcn_type->fcn.ret == exprtd);
  size_t argcnt = fcn_type->fcn.cnt;
  COMPILE_ASSERT(cnt == argcnt + 1);
  void * d = fcn_var->data;
  expr * result;
  switch(argcnt){
  case 0:
    result = ((expr * (*)())d)();
    break;
  case 1:
    result = ((expr * (*)(expr *))d)(exprs + 1);
    break;
  case 2:
    result = ((expr * (*)(expr *, expr *))d)(exprs + 1, exprs + 2);
    break;
  case 3:
    result = ((expr * (*)(expr *, expr *, expr *))d)(exprs + 1, exprs + 2, exprs + 3);
    break;
  case 4:
    result = ((expr * (*)(expr *, expr *, expr *, expr *))d)(exprs + 1, exprs + 2, exprs + 3, exprs + 4);
    break;
  default:
    ERROR("Not supported");
  }  
  return _compile_expr(block, val, *result);
}

expr walk_expr(expr body){
  if(body.type == VALUE)
    return body;
  
  sub_expr exp = body.sub_expr;
  if(exp.cnt > 0 && is_symbol(exp.exprs[0]) 
     && expr_symbol(exp.exprs[0]).id == get_symbol("unexpr").id){
    ASSERT(exp.cnt == 2);
    // it breaks here..
    expr * exp2 = lisp_compile_and_run_expr(exp.exprs[1]);
    return *exp2;
    // special case not handled yet.
  }
  expr sub[exp.cnt];
  for(size_t i = 0; i < exp.cnt; i++){
    sub[i] = walk_expr(exp.exprs[i]);
  }
  expr nexpr;
  nexpr.type = EXPR;
  nexpr.sub_expr.exprs = clone(sub,sizeof(sub));
  nexpr.sub_expr.cnt = exp.cnt;
  return nexpr;
}
expr * walk_expr2(expr * body){
  expr r = walk_expr(*body);
  return clone(&r, sizeof(r));
}
// expr turns makes a expr tree literal
type_def * expr_macro(c_block * block, c_value * val, expr body){
  UNUSED(block);
  type_def * exprtd = opaque_expr();
  char buf[30];
  static int expr_idx = 0;
  sprintf(buf,"_tmp_symbol_%i",expr_idx++);
  

  symbol tmp = get_symbol(buf);
  expr newbody = walk_expr(body);
  expr * ex = clone(&newbody, sizeof(expr));
  compiler_define_variable_ptr(tmp, exprtd, clone(&ex,sizeof(expr *)));
  
  expr e;
  e.type = VALUE;
  e.value.value = buf;
  e.value.strln = strlen(buf);
  e.value.type = SYMBOL;
  return _compile_expr(block, val, e);
}

type_def * defcmacro_macro(c_block * block, c_value * val, expr e_name, expr args, expr body){
  UNUSED(block);
  COMPILE_ASSERT(is_symbol(e_name));
  COMPILE_ASSERT(args.type == EXPR);

  type_def * exprtd = opaque_expr();

  size_t argcnt = args.sub_expr.cnt;
  expr * sexprs = args.sub_expr.exprs;
  symbol name = expr_symbol(e_name);
  logd("defining macro: '%s'\n", symbol_name(name));
  var_def _vars[argcnt];
  for(size_t i = 0; i < argcnt; i++){
    COMPILE_ASSERT(is_symbol(sexprs[i]));
    _vars[i].type = exprtd;
    _vars[i].name = expr_symbol(sexprs[i]);
    _vars[i].data = NULL;
  }

  c_root_code newfcn_root;
  newfcn_root.type = C_FUNCTION_DEF;
  c_fcndef * f = &newfcn_root.fcndef;
  c_block * blk = &f->block;
  c_value _val;
  var_def * __vars = _vars;
  
  with_symbols(&__vars, &argcnt, lambda(void, (){
	// this should happen at macro run time not macro compile time.
	type_def * td = _compile_expr(blk, &_val, body);
	if(td != exprtd){
	  logd("got '"); print_min_type(td); logd("' expected '");
	  print_min_type(exprtd); logd("'\n");
	  ERROR("Types does not match");
	}
      }));
  c_expr expr;
  expr.value = _val;
  expr.type = C_RETURN;
  blk->exprs = &expr;
  blk->expr_cnt = 1;
  
  type_def * args_type[argcnt];
  for(size_t i = 0; i < argcnt; i++){
    args_type[i] = exprtd;
  }
  type_def * fcnt = function_type(exprtd, argcnt, args_type);

  //decl *fdecl = &f->fdecl;
  f->name = name;
  f->type = fcnt;

  compile_as_c(&newfcn_root,1);
  return compile_value(val, string_expr(symbol_name(name)).value);
}

type_def * cast_macro(c_block * block, c_value * value, expr body, expr type){
  UNUSED(block);
  c_value * v = alloc0(sizeof(c_value));
  _compile_expr(block,v, body);
  type_def * cast_to = _type_macro(type);
  value->type = C_CAST;
  value->cast.value = v;
  value->cast.type = cast_to;
  return cast_to;
}

type_def * quote_macro(c_block * block, c_value * value, expr name){
  TEST_ASSERT(is_symbol(name));
  expr nexpr[2];
  nexpr[1] = name;
  nexpr[1].value.type = STRING;
  nexpr[0].value.type = SYMBOL;
  char * fcn= "get-symbol";
  nexpr[0].value.value = fcn;
  nexpr[0].value.strln = strlen(fcn); 
  expr pexpr;
  pexpr.type = EXPR;
  pexpr.sub_expr.exprs = nexpr;
  pexpr.sub_expr.cnt = array_count(nexpr);
  return _compile_expr(block, value, pexpr); 
}

type_def * defun_macro(c_block * block, c_value * value, expr name, expr args, expr body){

  // This function is rather complicated.
  // it handles turning something this: (defun funname (void (a i64) (b i64)) (+ a b)) 
  // into a function that can be called from througout the system.

  // there is really no simple way of doing this. <100 lines of code is ok for this task.
  // it generates a new c AST for defining the function and compiles it runtime.
  // it then registers the new function as a variable and returns the name of it.

  UNUSED(block);
  COMPILE_ASSERT(is_symbol(name));
  COMPILE_ASSERT(args.type == EXPR && args.sub_expr.cnt > 0);

  symbol fcnname = expr_symbol(name);
  logd("defining function: '%s'\n", symbol_name(fcnname));
  c_root_code newfcn_root;
  newfcn_root.type = C_FUNCTION_DEF;
  c_fcndef * f = &newfcn_root.fcndef;
  c_block * blk = &f->block;
  blk->exprs = NULL;
  blk->expr_cnt = 0;
  
  // ** get function decleration **
  
  f->name = fcnname;
  
  type_def * arg_types[args.sub_expr.cnt - 1];
  symbol arg_names[args.sub_expr.cnt - 1];
  
  for(size_t i = 0; i < args.sub_expr.cnt - 1; i++){
    expr arg = args.sub_expr.exprs[i + 1];
    COMPILE_ASSERT(arg.type == EXPR);
    COMPILE_ASSERT(arg.sub_expr.cnt == 2);
    expr namexpr = arg.sub_expr.exprs[0];
    expr typexpr = arg.sub_expr.exprs[1];
    COMPILE_ASSERT(is_symbol(namexpr));
    arg_names[i] = expr_symbol(namexpr);
    arg_types[i] = _type_macro(typexpr);
  }
  
  type_def * ret = _type_macro(args.sub_expr.exprs[0]);
  type_def * fcnt = function_type(ret, array_count(arg_names), (type_def **) arg_types);
  f->type = fcnt;
  // ** register arguments as symbols ** //
  size_t varcnt = array_count(arg_names);
  var_def _vars[varcnt];
  var_def * vars = _vars;
  for(size_t i = 0; i < varcnt; i++){
    vars[i].data = NULL;
    vars[i].name = arg_names[i];
    vars[i].type = arg_types[i];
  }
  
  // ** Compile body with symbols registered ** //
  c_value val;
  with_symbols(&vars, &varcnt, lambda(void, (){
	type_def * td = _compile_expr(blk,&val, body);
	ASSERT(td == fcnt->fcn.ret);
      }));
  c_expr expr;
  if(fcnt->fcn.ret == &void_def){
    expr.type = C_VALUE;
  }else{
    expr.type = C_RETURN;
  }
  expr.value = val;

  list_add((void **) &blk->exprs, &blk->expr_cnt, &expr, sizeof(c_expr));
  compile_as_c(&newfcn_root,1);
  // ** Just return the function name ** //
  return compile_value(value, string_expr(symbol_name(fcnname)).value);
}

void builtin_macros_load(){
  // Macros
  define_macro("type", 1, type_macro);
  define_macro("defun", 3, defun_macro);
  define_macro("var", 2, var_macro);
  define_macro("progn", -1, progn_macro);
  define_macro("cast", 2, cast_macro);
  define_macro("defvar", -1, defvar_macro);
  define_macro("load", 1, load_macro);
  define_macro("quote", 1, quote_macro);
  define_macro("setf", 2, setf_macro);
  define_macro("defcmacro", 3, defcmacro_macro);
  define_macro("expand",-1,expand_macro);
  define_macro("expr", 1, expr_macro);

  opaque_expr();
  compiler_define_variable_ptr(get_symbol("walk-expr"), 
			       str2type("(fcn (ptr expr) (a (ptr expr)))"), walk_expr2);
}
