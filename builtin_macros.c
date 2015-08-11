#include <stdio.h>
#include <iron/full.h>
#include "lisp_parser.h"
#include <libtcc.h>
#include "lisp_types.h"
#include "c_ast.h"
#include "lisp_std_types.h"
#include "lisp_compiler.h"
#include "type_pool.h"
#include "builtin_macros.h"
#include "expr_utils.h"
#include <stdlib.h>
type_def * setf_macro(type_def * expected_type, c_block * block, c_value * val, expr name, expr body);
bool lisp_print_errors = true;
type_def * opaque_expr(){
  static type_def * exprtd = NULL;
  if(exprtd == NULL){
    exprtd = str2type("(ptr (alias (opaque-struct _expr) expr))");
  }
  return exprtd;
}

type_def * no_op(type_def * expected_type, c_block * block, c_value * val){
  UNUSED(block);
  UNUSED(expected_type);
  val->type = C_NOTHING;
  return &void_def;
}
	  
type_def * type_macro(type_def * expected_type, c_block * block, c_value * value, expr e){
  CHECK_TYPE(expected_type, &type_def_ptr_def);
  static int typevarid = 0;
  type_def * t = expr2type(e);
  symbol varname = get_symbol_fmt("__type_%i",typevarid++);
  define_variable(varname, &type_def_ptr_def, t, false);
  type_def * rt = compile_expr(NULL, block, value, symbol_expr2(varname));
  ASSERT(rt == type_pool_get(&type_def_ptr_def));
  return rt;
}

expr mk_sub_expr(expr * exprs, size_t cnt){
  expr e;
  e.type = EXPR;
  e.sub_expr.exprs = exprs;
  e.sub_expr.cnt = cnt;
  return e;
}

// checks for conflict in argtypename and argname.
bool check_decl(symbol name, type_def * type){
  type_def * td = type_pool_simple(name);
  type_def * td2 = type;
  while(td2->type == POINTER){
    td2 = td2->ptr.inner;
  }
  if(td == td2){
    loge("Type/name conflict for variable named '%s'.\n", symbol_name(name));
    return false;
  }
  return true;
}

type_def * var_macro(type_def * expected_type, c_block * block, c_value * val, expr vars, expr body){

  COMPILE_ASSERT(vars.type == EXPR);
  sub_expr sexpr = vars.sub_expr;
  c_expr cvars[sexpr.cnt];

  var_def * lisp_vars = alloc(sizeof(var_def) * (sexpr.cnt + 1));
  for(size_t i = 0; i < sexpr.cnt; i++){
    c_value * cval = NULL;
    COMPILE_ASSERT(sexpr.exprs[i].type == EXPR);
    sub_expr var_expr = sexpr.exprs[i].sub_expr;
    
    
    c_var var;
    if(var_expr.cnt == 2){
      COMPILE_ASSERT(var_expr.exprs[0].type == VALUE 
		     && var_expr.exprs[0].value.type == SYMBOL);
      cval = alloc0(sizeof(c_value));
      var.var.type = compile_expr(NULL, block, cval, var_expr.exprs[1]);
      
    }else if(var_expr.cnt == 3){
      COMPILE_ASSERT(var_expr.exprs[0].type == VALUE 
		     && var_expr.exprs[1].type == VALUE 
		     && var_expr.exprs[0].value.type == SYMBOL);
      var.var.type = expr2type(var_expr.exprs[2]);
    }else{
      COMPILE_ERROR("Invalid var form");
    }
    
    var.var.name = expr_symbol(var_expr.exprs[0]);
    
    if(!check_decl(var.var.name, var.var.type))
      return error_def;
    var.value = cval;
    lisp_vars[i].name = var.var.name;
    lisp_vars[i].type = var.var.type;
    lisp_vars[i].data = NULL;
    cvars[i].type = C_VAR;
    cvars[i].var = var;
  }
  push_symbols(&lisp_vars, &sexpr.cnt);
  type_def * rettype = type_of(&body);
  bool is_void = rettype == &void_def;
  pop_symbols();
  
  c_expr sblk_expr;
  sblk_expr.type = C_BLOCK;
  sblk_expr.block = c_block_empty;

  c_expr tmpvar;
  if(!is_void){
    static i64 tmpid = 0;
    tmpvar.type = C_VAR;
    tmpvar.var.var.name = get_symbol_fmt("_tmpvar_%i", tmpid++);
    tmpvar.var.value = NULL;
    lisp_vars[sexpr.cnt].type = rettype;
    lisp_vars[sexpr.cnt].name = tmpvar.var.var.name;
  }

  for(size_t i = 0; i < sexpr.cnt; i++)
    block_add(&sblk_expr.block, cvars[i]);
  size_t varcnt = sexpr.cnt + (is_void ? 0 : 1);
  push_symbols(&lisp_vars, &varcnt);
  c_expr set_expr;
  set_expr.type = C_VALUE;
  type_def * ret_type = NULL;
  if(!is_void){
    ret_type = setf_macro(expected_type, &sblk_expr.block, &set_expr.value, symbol_expr2(tmpvar.var.var.name), body);
    tmpvar.var.var.type = ret_type;
  }else{
    ret_type = compile_expr(NULL, &sblk_expr.block, &set_expr.value, body);

  }
  block_add(&sblk_expr.block, set_expr);
  if(!is_void){
    block_add(block, tmpvar);
  }
  block_add(block, sblk_expr);
  if(!is_void){
    type_def * ret = compile_value(expected_type, val, symbol_expr2(tmpvar.var.var.name).value);
    COMPILE_ASSERT(ret == ret_type);
  }else{
    val->type = C_NOTHING;
  }
  pop_symbols();

  dealloc(lisp_vars);
  return ret_type;
}

type_def * defvar_macro(type_def * expected_type, c_block * block, 
			c_value * val, expr * exprs, size_t cnt){
  COMPILE_ASSERT(cnt == 2 || cnt == 3);
  expr name = exprs[0];
  COMPILE_ASSERT(is_symbol(name));
  symbol sym = expr_symbol(name);
  if(!is_check_type_run())
    logd("Defining variable '%s'.\n", symbol_name(sym));
  type_def * t;

  if(is_keyword(exprs[1]) && expr_symbol(exprs[1]).id == get_symbol("type").id && cnt == 3){
    
    t = expr2type(exprs[2]);
    val->type = C_SYMBOL;
    val->symbol = sym;
    if(is_check_type_run()){
      return t;
    }
  }else{
    COMPILE_ASSERT(cnt == 2);
    expr body = exprs[1];
    c_value * vr = alloc0(sizeof(c_value));
    c_value * vl = alloc0(sizeof(c_value));
    vl->type = C_SYMBOL;
    vl->symbol = sym;
    t = compile_expr(expected_type, block, vr, body);
    if(is_check_type_run()){
      return t;
    }
    COMPILE_ASSERT(t != error_def);
    val->type = C_OPERATOR;
    val->operator.left = vl;
    val->operator.right = vr;
    val->operator.operator = "=";
  }
  
  c_root_code var_root;
  var_root.type = C_VAR_DEF;
  var_root.var.var.name = sym;
  var_root.var.var.type = t;
  var_root.var.value = NULL;
  void * codebuf = compile_as_c(&var_root,1);
  UNUSED(codebuf);
  c_root_code_delete(var_root);
  return t;
}

type_def * setf_macro(type_def * expected_type, c_block * block, c_value * val, expr name, expr body){
  c_value * vr = alloc0(sizeof(c_value));
  c_value * vl = alloc0(sizeof(c_value));
  type_def * t1 = compile_expr(expected_type, block, vl, name);
  type_def * t = compile_expr(t1, block, vr, body);
  COMPILE_ASSERT(t1 != error_def && t != error_def);
  if(t != t1){
    loge("same types required for setf. Requires '");
    print_min_type(t1);
    loge("' got '");
    print_min_type(t);
    loge("'.\n");
    
    logd("at:\n");
    print_expr(&name);
    logd("\nexpr:\n");
    print_expr(&body);
    logd("\n");
    COMPILE_ERROR("cannot implicitly convert type");
  }
  val->type = C_OPERATOR;
  val->operator.left = vl;
  val->operator.right = vr;
  val->operator.operator = "=";
  return t;
}

type_def * load_macro(type_def * expected_type, c_block * block, c_value * val, expr file_name){
  COMPILE_ASSERT(is_string(file_name));
  char * filename = read_symbol(file_name);
  logd("Loading: '%s'\n", filename); 
  compile_status s = lisp_run_script_file(filename);
  dealloc(filename);
  
  if(s == COMPILE_ERROR){
    COMPILE_ERROR("Unable to compile and run '%s'.", filename);
  }
  return compile_expr(expected_type, block, val, file_name);
}

type_def * progn_macro(type_def * expected_type, c_block * block, c_value * val, expr * expressions, size_t expr_cnt){
  type_def * d;
  type_def * exp = NULL;
  for(size_t i = 0; i < expr_cnt; i++){
    c_value _val = c_value_empty;
    if(expr_cnt - 1 == i)
      exp = expected_type;
    d = compile_expr(exp, block, &_val, expressions[i]);
   
    COMPILE_ASSERT(d != error_def);
    if(i == expr_cnt -1){
      *val = _val;
      return d;
    }
    c_expr expr;
    expr.type = C_VALUE;
    expr.value = _val;
    block_add(block, expr);
  }
  if(expr_cnt == 0)
    val->type = C_NOTHING;
  return &void_def;
}
	  
typedef struct{
  symbol fcn;
  bool rest;
}macro_store;

type_def * macro_store_type(){
  return str2type("(ptr (alias (opaque-struct _macro_store) macro_store))");
}

void print_macro_store(macro_store * ms){
  logd("symbol : %s, is rest?: %i\n", symbol_name(ms->fcn), ms->rest);
}

expr * expand_macro_store(type_def * expected_type, macro_store * ms, expr * exprs, size_t cnt){
  var_def * v = get_global(ms->fcn);
  type_def * t = v->type;
  ASSERT(t->type == FUNCTION);
  ASSERT(t->fcn.ret == opaque_expr());
  
  if(v->type->fcn.args[0] == type_pool_get(&type_def_ptr_def)){
    ASSERT(v->type->fcn.cnt == 2);
    expr * (* d)(type_def * ex, expr * expr) = v->data;
    expr last_sub_expr;
    last_sub_expr.type = EXPR;
    last_sub_expr.sub_expr.exprs = exprs ;
    last_sub_expr.sub_expr.cnt = cnt;
    return d(expected_type, &last_sub_expr);
  }

  if(!(t->fcn.cnt == (i32)cnt || (ms->rest && t->fcn.cnt <= (i32)cnt))){
    ERROR("Unvalid number of arguments %i for macro function '%s' expected %i.", t->fcn.cnt, symbol_name(ms->fcn), cnt);
    return NULL;
  }
  
  expr * (* d)(expr * e, ...) = v->data;
  expr * (* d0)() = v->data;
  if(ms->rest == false){
    switch(cnt){
    case 0:
      return d0();
    case 1:
      return d(exprs);
    case 2:
      return d(exprs, exprs + 1);
    case 3:
      return d(exprs, exprs + 1, exprs + 2);
    case 4:
      return d(exprs, exprs + 1, exprs + 2, exprs + 3);
    case 5:
      return d(exprs, exprs + 1, exprs + 2, exprs + 3, exprs + 4);
    case 6:
      return d(exprs, exprs + 1, exprs + 2, exprs + 3, exprs + 4, exprs + 5);
    case 7:
      return d(exprs, exprs + 1, exprs + 2, exprs + 3, exprs + 4, exprs + 5, exprs + 6);
    case 8:
      return d(exprs, exprs + 1, exprs + 2, exprs + 3, exprs + 4, exprs + 5, exprs + 6, exprs + 7);
    default:
      ERROR("Unsupported number of macro args");
    }
  }else{
    size_t extra_args = cnt - t->fcn.cnt + 1;
    size_t offset = t->fcn.cnt - 1;
    expr last_sub_expr;
    last_sub_expr.type = EXPR;
    last_sub_expr.sub_expr.exprs = exprs + offset;
    last_sub_expr.sub_expr.cnt = extra_args;
      
    switch(t->fcn.cnt){
    case 1:
      return d(&last_sub_expr);
    case 2:
      return d(exprs, &last_sub_expr);
    case 3:
      return d(exprs, exprs + 1, &last_sub_expr);
    case 4:
      return d(exprs, exprs + 1, exprs + 2, &last_sub_expr);
    case 5:
      return d(exprs, exprs + 1, exprs + 2, exprs + 3, &last_sub_expr);
    case 6:
      return d(exprs, exprs + 1, exprs + 2, exprs + 3, exprs + 4, &last_sub_expr);
    case 7:
      return d(exprs, exprs + 1, exprs + 2, exprs + 3, exprs + 4, exprs + 5, &last_sub_expr);
    case 8:
      return d(exprs, exprs + 1, exprs + 2, exprs + 3, exprs + 4, exprs + 5, exprs + 6, &last_sub_expr);
    default:
      ERROR("Unsupported number of macro args");
    }
  }
  return NULL;
}

expr * expand_macro_store2(macro_store * ms, expr * expr){
  return expand_macro_store(opaque_expr(), ms, expr->sub_expr.exprs, expr->sub_expr.cnt);
}

type_def * expand_macro(type_def * expected_type, c_block * block, c_value * val, 
			expr * exprs, size_t cnt){
  COMPILE_ASSERT(cnt > 0);
  COMPILE_ASSERT(is_symbol(exprs[0]));
  
  type_def * exprtd = macro_store_type();

  symbol name = expr_symbol(exprs[0]);
  var_def * fcn_var = get_any_variable(name);
  COMPILE_ASSERT(fcn_var != NULL);
  COMPILE_ASSERT(fcn_var->type == exprtd);
  expr * outexpr = expand_macro_store(expected_type, fcn_var->data, exprs + 1, cnt - 1);
  // Note: nothing going in or out from expand_macro_store can be deleted. 
  // Anything going in is already deleted later, stuff going out might be. 
  // The rest could be deleted, but user has to mark for deletion.
  // furthermore there is a potential bug connected to last_sub_expr (see expand_macro_store)/
  
  if(outexpr == NULL)
    COMPILE_ERROR("Unable to expand macro '%s'.", symbol_name(name));
  
  return compile_expr(expected_type, block, val, *outexpr);
}

int recurse_count(expr ex){
  if(ex.type == VALUE)
    return 0;
  sub_expr exp = ex.sub_expr;
  if(exp.cnt > 0 && is_symbol(exp.exprs[0]) 
     && expr_symbol(exp.exprs[0]).id == get_symbol("unexpr").id){
    return 1;
  }
  
  if(exp.cnt > 0 && is_symbol(exp.exprs[0]) 
     && expr_symbol(exp.exprs[0]).id == get_symbol("expr").id){
    //ASSERT(exp.cnt == 2);
    return 0;
  }

  int cnt = 0;
  for(size_t i = 0; i < exp.cnt; i++)
    cnt += recurse_count(exp.exprs[i]);    
  return cnt;
}

expr recurse_expand(expr ex, expr ** exprs, int * cnt){
  if(ex.type == VALUE)
    return clone_expr2(ex);
  sub_expr exp = ex.sub_expr;
  if(exp.cnt > 0 && is_symbol(exp.exprs[0]) 
     && expr_symbol(exp.exprs[0]).id == get_symbol("unexpr").id){
    ASSERT(exp.cnt == 2);
    int idx = *cnt;
    (*cnt)++;
    return clone_expr2(*exprs[idx]);
  }else if(exp.cnt > 0 && is_symbol(exp.exprs[0]) 
	   && expr_symbol(exp.exprs[0]).id == get_symbol("expr").id){
    return clone_expr2(ex);
  }
  expr sexp[exp.cnt];
  for(size_t i = 0; i < exp.cnt; i++)
    sexp[i] = recurse_expand(exp.exprs[i], exprs, cnt);
  expr new;
  new.type = EXPR;
  new.sub_expr.exprs = clone(sexp, sizeof(sexp));
  new.sub_expr.cnt = exp.cnt;
  return new;
}
#include <stdarg.h>
expr * expand_expr(expr * exprs, ...){
  expr * first = exprs;
  int nexprs = recurse_count(*first);
  int cnt = 0;
  va_list vl;
  va_start(vl, exprs); 
  expr * list[nexprs];
  for(int i = 0; i < nexprs; i++)
    list[i] =va_arg(vl, expr *);
  va_end(vl);
  expr _new = recurse_expand(*first, list, &cnt);
  return clone(&_new, sizeof(expr));
}

symbol get_recurse_sym(int id, int cnt){
  return get_symbol_fmt("_tmp_%i__%i_", id, cnt);
}

bool recurse_expr(expr * ex, c_block * block, int id, int * cnt, var_def ** vars){
  if(ex->type == VALUE)
    return true;
  sub_expr exp = ex->sub_expr;
  if(exp.cnt > 0 && is_symbol(exp.exprs[0]) 
     && expr_symbol(exp.exprs[0]).id == get_symbol("unexpr").id){
    if(exp.cnt != 2)
      return false;
    
    c_value cval;
    type_def * t = compile_expr(opaque_expr(), block, &cval, exp.exprs[1]);
    if(t == error_def){
      return false;
    }
    
    var_def vard;
    vard.type = t;
    vard.name = get_recurse_sym(id,(*cnt)++);
    vard.data = NULL;
    
    size_t scnt = *cnt - 1;
    list_add((void **) vars, &scnt, &vard, sizeof(vard));
    
    c_var var;
    var.var.name = vard.name;
    var.var.type = t;
    var.value = clone(&cval, sizeof(cval));
    c_expr exp;
    exp.type = C_VAR;
    exp.var = var;
    block_add(block, exp);
    return true;
  }else if(exp.cnt > 0 && is_symbol(exp.exprs[0]) 
	   && expr_symbol(exp.exprs[0]).id == get_symbol("expr").id){
    return true;
  }else{
    bool ok = true;
    for(size_t i = 0; i < exp.cnt; i++)
      ok = ok && recurse_expr(exp.exprs + i, block, id, cnt, vars);
    return ok;	
  }
}

type_def * expr_macro(type_def * expected_type, c_block * block, c_value * val, expr body){
  static int _id = 0;
  _id++;
  int id = _id; 
  type_def * exprtd = opaque_expr();	  
  
  symbol tmp = get_symbol_fmt("_tmp_symbol_%i", id);
  expr * ex = clone_expr(&body);
  int cnt = 0;
  var_def * vars = NULL;
  bool ok = recurse_expr(ex, block, id, &cnt, &vars);
  if(!ok) COMPILE_ERROR("Error while storing expression tree.");
  
  define_variable(tmp, exprtd, ex, false);
  
  type_def ftype = *str2type("(fcn (ptr expr) (e (ptr expr)) )");
  type_def * args[cnt + 2];
  for(int i = 0; i < cnt + 2; i++)
    args[i] = exprtd;
  ftype.fcn.args = args;
  ftype.fcn.cnt = cnt + 1;

  type_def * ftype2 = type_pool_get(&ftype);
  symbol expandname = get_symbol_fmt("___expand%i", cnt);
  if(get_global(expandname) == NULL)
    defun(symbol_name(expandname), ftype2, expand_expr);

  expr callexprs[cnt + 2];
  callexprs[0] = symbol_expr2(expandname);
  callexprs[1] = symbol_expr2(tmp);
  for(int i = 0; i < cnt; i++)
    callexprs[i + 2] = symbol_expr2(get_recurse_sym(id, i));
  expr sexp;
  sexp.type = EXPR;
  sexp.sub_expr.exprs = callexprs;
  sexp.sub_expr.cnt = cnt + 2;
  size_t vars_cnt = cnt;
  push_symbols(&vars, &vars_cnt);
  type_def * td = compile_expr(expected_type, block, val,  sexp);
  pop_symbols();
  list_clean((void **) &vars, &vars_cnt);
  return td;
}

// Just a code sanity check. Will generate an error if unexpr is used outside expr.
type_def * unexpr_macro(type_def * expected_type, c_block * block, c_value * val, expr body){
  UNUSED(expected_type);
  UNUSED(block);UNUSED(val);UNUSED(body);
  COMPILE_ERROR("Calls to unexpr must be nested inside an expr body\n");
}

type_def * declare_macro_macro(type_def * expected_type, c_block * block, c_value * val, 
			       expr * exprs, size_t cnt){
  CHECK_TYPE(expected_type, char_ptr_def);
  UNUSED(block);
  ASSERT(cnt == 2 || cnt == 3);
  expr macro_name = exprs[0];
  expr function_name = exprs[1];
  ASSERT(is_symbol(macro_name));
  ASSERT(is_symbol(function_name));
  
  macro_store * macro = alloc0(sizeof(macro_store));
  macro->fcn = expr_symbol(function_name);
  macro->rest = cnt == 3;
  define_variable(expr_symbol(macro_name), macro_store_type() , macro, false);
  return compile_value(expected_type, val, string_expr(read_symbol(macro_name)).value);
}

// Casts a variable to a new type.
type_def * cast_macro(type_def * expected_type, c_block * block, c_value * value, 
		      expr body, expr type){
  c_value * v = alloc0(sizeof(c_value));
  type_def * td = compile_expr(NULL, block, v, body);
  COMPILE_ASSERT(td != error_def);	  
  type_def * cast_to = expr2type(type);
  CHECK_TYPE(expected_type, cast_to);
  value->type = C_CAST;
  value->cast.value = v;
  value->cast.type = cast_to;
  return cast_to;
}

// Ensures that the type constraints knows which type this should become.
// Kind of the opposite of what cast does.
type_def * the_macro(type_def * expected_type, c_block * block, c_value * value, 
		      expr body, expr type){
  type_def * type_constraint = expr2type(type);
  COMPILE_ASSERT(type_constraint != error_def);
  CHECK_TYPE(expected_type, type_constraint);
  type_def * td = compile_expr(type_constraint, block, value, body);
  COMPILE_ASSERT(td != error_def);
  COMPILE_ASSERT(td == type_constraint);
  return td;
}

type_def * stringify_macro(type_def * expected_type, c_block * block, c_value * value, expr str){
  TEST_ASSERT(is_symbol(str));
  str.value.type = STRING;
  return compile_expr(expected_type, block, value, str);
}

type_def * defun_macro(type_def * expected_type, c_block * block, c_value * value, expr name, expr args, expr body){
  CHECK_TYPE(expected_type, char_ptr_def);
  if(!lisp_print_errors) return char_ptr_def;
  // This function is rather complicated.
  // it handles turning something this: (defun funname (void (a i64) (b i64)) (+ a b)) 
  // into a function that can be called from througout the system.
  // there is really no simple way of doing this. <100 lines of code is ok for this task.
  // it generates a new c AST for defining the function and compiles it runtime.
  // it then registers the new function as a variable and returns the name of it.

  COMPILE_ASSERT(is_symbol(name));
  COMPILE_ASSERT(args.type == EXPR && args.sub_expr.cnt > 0);

  symbol fcnname = expr_symbol(name);
  logd("Defining function: '%s'.\n", symbol_name(fcnname));
  c_root_code newfcn_root;
  newfcn_root.type = C_FUNCTION_DEF;
  c_fcndef * f = &newfcn_root.fcndef;
  c_block * blk = &f->block;
  blk->exprs = NULL;
  blk->expr_cnt = 0;
  
  // ** get function decleration ** //  
  f->name = fcnname;
  
  type_def * arg_types[args.sub_expr.cnt - 1];
  symbol arg_names[args.sub_expr.cnt - 1];
  f->args = arg_names;
  for(size_t i = 0; i < args.sub_expr.cnt - 1; i++){
    expr arg = args.sub_expr.exprs[i + 1];
    if(arg.type != EXPR || arg.sub_expr.cnt != 2){
      COMPILE_ERROR("Arguments must be of the format (name type). This is the case for %i.", i);
    }
    expr namexpr = arg.sub_expr.exprs[0];
    expr typexpr = arg.sub_expr.exprs[1];
    COMPILE_ASSERT(is_symbol(namexpr));
    arg_names[i] = expr_symbol(namexpr);
    arg_types[i] = expr2type(typexpr);
  }
  
  type_def * ret = expr2type(args.sub_expr.exprs[0]);
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
    if(!check_decl(vars[i].name, vars[i].type)){
      return error_def;
    }
  }
  
  // ** Compile body with symbols registered ** //
  c_value val = c_value_empty;
  push_symbols(&vars, &varcnt);
  type_def * td = compile_expr(ret, blk, &val, body);
  pop_symbols();
  if(td == error_def){
    loge("Caught error while defining function '%s'.\n", symbol_name(fcnname));
    return td;
  }
  if(fcnt->fcn.ret != &void_def && td != fcnt->fcn.ret){
    loge("Error while defining function: ");
    logd("Function declared to return \n\"");
    print_min_type(fcnt->fcn.ret);
    logd("\"\nfunction body returns \n\"");
    print_min_type(td);
    logd("\"\n\n");
    return error_def;
  }
  
  c_expr expr;
  if(fcnt->fcn.ret == &void_def){
    expr.type = C_VALUE;
  }else{
    expr.type = C_RETURN;
  }
  expr.value = val;
  block_add(blk,expr);
  compile_as_c(&newfcn_root,1);
  c_root_code_delete(newfcn_root);
  // ** Just return the function name ** //
  return compile_expr(char_ptr_def, block, value, string_expr(symbol_name(fcnname)));
}

type_def * math_operator(char * operator, type_def * expected_type, c_block * block, c_value * val, expr item1, expr item2){
  if(expected_type != NULL)
    COMPILE_ASSERT(is_number_type(expected_type));
  c_value * val1 = alloc0(sizeof(c_value));
  c_value * val2 = alloc0(sizeof(c_value));
  c_value * comp = val;
  type_def * t1 = compile_expr(expected_type, block, val1, item1);
  COMPILE_ASSERT(t1 != error_def && t1 != &void_def);
  COMPILE_ASSERT(is_number_type(t1));

  type_def * t2 = compile_expr(expected_type, block, val2, item2);
  COMPILE_ASSERT(is_number_type(t2));
  CHECK_TYPE(expected_type,t1);
  CHECK_TYPE(expected_type,t2);
  
  comp->type = C_OPERATOR;
  comp->operator.operator = operator;
  comp->operator.left = val1;
  comp->operator.right = val2;
  return t1;
}

type_def * plus_macro(type_def * expected_type, c_block * block, c_value * val, expr item1, expr item2){
  return math_operator("+", expected_type, block, val, item1, item2);
}

type_def * minus_macro(type_def * expected_type, c_block * block, c_value * val, expr item1, expr item2){
  return math_operator("-", expected_type, block, val, item1, item2);
}

type_def * multiply_macro(type_def * expected_type, c_block * block, c_value * val, expr item1, expr item2){
  return math_operator("*", expected_type, block, val, item1, item2);
}

type_def * divide_macro(type_def * expected_type, c_block * block, c_value * val, expr item1, expr item2){
  return math_operator("/", expected_type, block, val, item1, item2);
}

type_def * modulus_macro(type_def * expected_type, c_block * block, c_value * val, expr item1, expr item2){
  return math_operator("%", expected_type, block, val, item1, item2);
}


type_def * comparison_macro(char * operator, type_def * expected_type, c_block * block, c_value * val, expr item1, expr item2){
  CHECK_TYPE(expected_type, &bool_def);
  c_value * val1 = alloc0(sizeof(c_value));
  c_value * val2 = alloc0(sizeof(c_value));
  c_value * comp = alloc0(sizeof(c_value));
  type_def * t1 = type_of2(NULL,&item1);//
  type_def * t2 = type_of2(t1,&item2);//compile_expr(t1, block, val2, item2);
  if(t2 != t1){
    t2 = type_of2(NULL,&item2);    
    t1 = type_of2(t2, &item1);
  }
  COMPILE_ASSERT(t1 != error_def && t1 != &void_def);
  COMPILE_ASSERT(t1 == t2);
  COMPILE_ASSERT(t1 == compile_expr(t1, block, val1, item1));
  COMPILE_ASSERT(t2 == compile_expr(t2, block, val2, item2));
  val->type = C_CAST;
  val->cast.value = comp;
  val->cast.type = str2type("bool");
  comp->type = C_OPERATOR;
  comp->operator.operator = operator;
  comp->operator.left = val1;
  comp->operator.right = val2;
  return val->cast.type;
}

type_def * eq_macro(type_def * expected_type, c_block * block, c_value * val, expr item1, expr item2){
  return comparison_macro("==", expected_type, block, val, item1, item2);
}

type_def * less_than_macro(type_def * expected_type, c_block * block, c_value * val, expr item1, expr item2){
  return comparison_macro("<", expected_type, block, val, item1, item2);
}

type_def * bigger_than_macro(type_def * expected_type, c_block * block, c_value * val, expr item1, expr item2){
  return comparison_macro(">", expected_type, block, val, item1, item2);
}

type_def * leq_macro(type_def * expected_type, c_block * block, c_value * val, expr item1, expr item2){
  return comparison_macro("<=", expected_type, block, val, item1, item2);
}

type_def * beq_macro(type_def * expected_type, c_block * block, c_value * val, expr item1, expr item2){
  return comparison_macro(">=", expected_type, block, val, item1, item2);
}

type_def * if_atom_macro(type_def * expected_type, c_block * block, c_value * val, expr cnd, expr then, expr _else){
  CHECK_TYPE(expected_type, &void_def);
  expected_type = NULL;
  
  c_expr cmpexpr;
  cmpexpr.value = c_value_empty;
  cmpexpr.type = C_VALUE_UNENDED;
  type_def * cmp = compile_expr(&bool_def, block, &cmpexpr.value, cnd);

  if(cmp != &bool_def)
    COMPILE_ERROR("'if' comparison expression must return a boolean value.");
  cmpexpr.value = c_value_sub_expr(clone(&cmpexpr.value, sizeof(c_value)));  
  c_expr then_blk_expr = c_expr_block;
  c_expr then_expr = c_expr_value;
	  
  c_expr else_blk_expr = c_expr_block;
  c_expr else_expr = c_expr_value;
  then_expr.type = C_VALUE;
  
  type_def * td1 = compile_expr(expected_type, &then_blk_expr.block, &then_expr.value, then);
  
  COMPILE_ASSERT(td1 != error_def);
  type_def * td2 = compile_expr(expected_type, &else_blk_expr.block, &else_expr.value, _else);
  COMPILE_ASSERT(td2 != error_def);
  block_add(&else_blk_expr.block, else_expr);
  block_add(&then_blk_expr.block, then_expr);
  block_add(block, c_expr_keyword("if"));
  block_add(block, cmpexpr);
  block_add(block, then_blk_expr);
  block_add(block, c_expr_keyword("else"));
  block_add(block, else_blk_expr);
  val->type = C_NOTHING;
  return &void_def;
}

type_def * while_atom_macro(type_def * expected_type, c_block * block, c_value * val, expr cnd, expr body){
  CHECK_TYPE(expected_type, &void_def);
  
  c_expr whilexpr = c_expr_keyword("while");
  c_expr cmpexpr;
  cmpexpr.value = c_value_empty;
  cmpexpr.type = C_VALUE_UNENDED;
  type_def * cmp = compile_expr(&bool_def, block, &cmpexpr.value, cnd);
  COMPILE_ASSERT(cmp == &bool_def);
  cmpexpr.value = c_value_sub_expr(clone(&cmpexpr.value, sizeof(c_value)));  
  
  c_expr bodyexpr;
  bodyexpr.type = C_BLOCK;
  bodyexpr.block = c_block_empty;
  c_expr valuexpr;
  valuexpr.type = C_VALUE;
  type_def * t = compile_expr(NULL, &bodyexpr.block, &valuexpr.value, body);
  COMPILE_ASSERT(t != error_def);
  block_add(&bodyexpr.block, valuexpr);
  block_add(block, whilexpr);
  block_add(block, cmpexpr);
  block_add(block, bodyexpr);
  val->type = C_NOTHING;
  return &void_def;
}

type_def * deref_macro(type_def * expected_type, c_block * block, c_value * val, expr ptr){
  c_value * _val = new(c_value);
  if(expected_type != NULL)
    expected_type = make_ptr(expected_type);
  type_def * td = compile_expr(expected_type, block, _val, ptr);
  COMPILE_ASSERT(td != error_def && td->type == POINTER);
  val->type = C_DEREF;
  val->deref.inner = _val;
  val->deref.return_type = td->ptr.inner;
  return td->ptr.inner;
}

type_def * addrof_macro(type_def * expected_type, c_block * block, c_value * val, expr value){
  if(expected_type != NULL){
    COMPILE_ASSERT(expected_type->type == POINTER);
    expected_type = expected_type->ptr.inner;
  }
  c_value * _val = new(c_value);
  type_def * td = compile_expr(expected_type, block, _val, value);
  COMPILE_ASSERT(td != error_def);
  val->type = C_ADDRESS_OF;
  val->value = _val;
  return type_pool_get(make_ptr(td));
}

expr * number2expr(i64 num){
  char * str = fmtstr("%i",num);
  expr e;
  e.type = VALUE;
  e.value.type = NUMBER;
  e.value.value = str;
  e.value.strln = strlen(str);
  return clone(&e, sizeof(e));
}

i64 expr2number(expr * e){
  ASSERT(e->type == VALUE && e->value.type == NUMBER);
  char buf[e->value.strln + 1];
  buf[e->value.strln] = 0;
  memcpy(buf, e->value.value, e->value.strln);
  char * endptr = NULL;
  return strtoll(buf, &endptr, 10);
}

type_def * boolean_operator(char * operator, type_def * expected_type, c_block * blk, c_value * val, expr left, expr right){
  c_value left_value, right_value;
  type_def * left_t = compile_expr(expected_type, blk,&left_value,left);
  type_def * right_t = compile_expr(expected_type, blk,&right_value,right);
  COMPILE_ASSERT(left_t == &bool_def && left_t == right_t);
  val->type = C_OPERATOR;
  val->operator.left = clone(&left_value, sizeof(c_value));
  val->operator.right = clone(&right_value, sizeof(c_value));
  val->operator.operator = operator;
  return left_t;
}

type_def * bitwise_operator(char * operator, type_def * expected_type, c_block * blk, c_value * val, expr left, expr right){
  if(expected_type != NULL)
    COMPILE_ASSERT(is_integer_type(expected_type));
  c_value * left_value = alloc(sizeof(c_value)), * right_value = alloc(sizeof(c_value));
  type_def * left_t = compile_expr(expected_type, blk,left_value,left);
  type_def * right_t = compile_expr(left_t, blk,right_value,right);
  COMPILE_ASSERT(is_integer_type(left_t) && left_t == right_t);
  val->type = C_OPERATOR;
  val->operator.left = left_value;
  val->operator.right = right_value;
  val->operator.operator = operator;
  return left_t;
}

type_def * bitor_operator(type_def * expected_type, c_block * blk, c_value * val, expr left, expr right){
  return bitwise_operator("|", expected_type, blk, val, left, right);
}

type_def * bitand_operator(type_def * expected_type, c_block * blk, c_value * val, expr left, expr right){
  return bitwise_operator("&", expected_type, blk, val, left, right);
}

type_def * bit_leftshift_operator(type_def * expected_type, c_block * blk, c_value * val, expr left, expr right){
  return bitwise_operator("<<", expected_type, blk, val, left, right);
}

type_def * bit_rightshift_operator(type_def * expected_type, c_block * blk, c_value * val, expr left, expr right){
  return bitwise_operator(">>", expected_type, blk, val, left, right);
}

type_def * and_macro(type_def * expected_type, c_block * blk, c_value * val, expr left, expr right){
  return boolean_operator("&&", expected_type, blk, val, left, right);
}

type_def * or_macro(type_def * expected_type, c_block * blk, c_value * val, expr left, expr right){
  return boolean_operator("||", expected_type, blk, val, left, right);
}

type_def * member_macro(type_def * expected_type, c_block * blk, c_value * val, expr object, expr member){
  UNUSED(expected_type);
  ASSERT(is_symbol(member));
  val->type = C_MEMBER;
  val->member.name = expr_symbol(member);
  val->member.item = alloc0(sizeof(c_value));
  type_def * obj_type = compile_expr(NULL, blk, val->member.item, object);
  while(obj_type->type == TYPEDEF) obj_type = obj_type->ctypedef.inner;
  if(obj_type->type != STRUCT && obj_type->type != UNION){
    loge("Error: Expected objects of type struct or union. got %i\n", obj_type->type);
    ERROR("Unsupported type");
  }
  type_def * memtype = NULL;
  for(int i = 0; i < obj_type->cstruct.cnt; i++){
    decl member = obj_type->cstruct.members[i];
    if(member.name.id == val->member.name.id){
      memtype = member.type;
    }
  }
  
  if(memtype == NULL)
    ERROR("Unable to find member '%s' on struct", symbol_name(val->member.name));
  val->member.type = memtype;
  return memtype;
}

symbol * expr2symbol(expr * e){
  char buffer[e->value.strln + 1];
  memcpy(buffer, e->value.value, e->value.strln);
  buffer[e->value.strln] = 0;
  return get_symbol2(buffer);
}

expr * symbol2expr(symbol * s){
  expr * out = alloc(sizeof(expr));
  out->type = VALUE;
  out->value.type = SYMBOL;
  out->value.value = symbol_name(*s);
  out->value.strln = strlen(out->value.value);
  return out;
}

bool is_expr_symbol(expr * e){
  return e->type == VALUE && e->value.type == SYMBOL;
}
	  
bool is_sub_expr(expr * e){
  return e->type == EXPR;
}

u64 get_sub_expr_cnt(expr * e){
  return e->sub_expr.cnt;
}

expr * get_sub_expr(expr * e, u64 idx){
  if(e->type == EXPR && e->sub_expr.cnt > idx)
    return e->sub_expr.exprs + idx;
  return NULL;
}

expr * make_sub_expr (expr ** exprs, u64 cnt){
  expr * e = new(expr);
  e->type = EXPR;
  e->sub_expr.cnt = cnt;
  e->sub_expr.exprs = alloc(sizeof(expr) * cnt);
  for(u32 i = 0; i < cnt ; i++)
    e->sub_expr.exprs[i] = *exprs[i];//clone_expr2(*exprs[i]);
  return e;
}

expr * sub_expr_skip(expr * e){
  if(e->type != EXPR || e->sub_expr.cnt == 0)
    return NULL;
  expr * e2 = new(expr);
  e2->sub_expr.exprs = e->sub_expr.exprs + 1;
  e2->sub_expr.cnt = e->sub_expr.cnt - 1;
  e2->type = EXPR;
  return e2;
}

expr * gensym(){
  static int symid = 0;
  expr e = symbol_expr2(get_symbol_fmt("__sym_%i", symid++));
  return (expr *) clone(&e, sizeof(e));
}

void free_expr(expr * e){
  delete_expr(e);
  dealloc(e);
}

i64 macro_store_args(macro_store * ms){
  var_def * v = get_global(ms->fcn);
  if(v == NULL)
    return 0;
  ASSERT(v->type->type == FUNCTION);
  return ms->rest ? -1 : v->type->fcn.cnt;
}

void builtin_macros_load(){
  // Macros
  macro_store_type();
  define_macro("type", 1, type_macro);
  define_macro("defun", 3, defun_macro);
  define_macro("var", 2, var_macro);
  define_macro("progn", -1, progn_macro);
  define_macro("cast", 2, cast_macro);
  define_macro("the", 2, the_macro);
  define_macro("defvar", -1, defvar_macro);
  define_macro("load", 1, load_macro);
  define_macro("stringify", 1, stringify_macro);
  define_macro("setf", 2, setf_macro);

  define_macro("declare-macro", -1, declare_macro_macro);
  define_macro("expand",-1,expand_macro);
  define_macro("expr", 1, expr_macro);
  define_macro("eq", 2, eq_macro);
  define_macro("==", 2, eq_macro);
  define_macro("<", 2, less_than_macro);
  define_macro(">", 2, bigger_than_macro);
  define_macro("<=", 2, leq_macro);
  define_macro(">=", 2, beq_macro);
  define_macro("if!", 3, if_atom_macro);

  define_macro("bit-or", 2, bitor_operator);
  define_macro("bit-and", 2, bitand_operator);
   define_macro("<<", 2, bit_leftshift_operator);
  define_macro(">>", 2, bit_rightshift_operator);

  define_macro("while!", 2, while_atom_macro);
  define_macro("deref", 1, deref_macro);
  define_macro("addrof", 1, addrof_macro);
  define_macro("noop",0,no_op);
  define_macro("and",2,and_macro);
  define_macro("or",2,or_macro);
  define_macro("member", 2, member_macro);
  define_macro("unexpr", 1, unexpr_macro);
  define_macro(".+", 2, plus_macro);
  define_macro(".-", 2, minus_macro);
  define_macro(".*", 2, multiply_macro);  
  define_macro("./", 2, divide_macro);
  define_macro(".%", 2, modulus_macro);

  opaque_expr();
  defun("number2expr",str2type("(fcn (ptr expr) (a i64))"), number2expr);
  defun("expr2number",str2type("(fcn i64 (a (ptr expr)))"), expr2number);
  defun("expr2symbol", str2type("(fcn (ptr symbol) (a (ptr expr)))"), expr2symbol);
  defun("symbol2expr", str2type("(fcn (ptr expr) (a (ptr symbol)))"), symbol2expr);
  
  defun("is-sub-expr", str2type("(fcn bool (expr (ptr expr)))"), is_sub_expr);
  defun("sub-expr.cnt", str2type("(fcn u64 (expr (ptr expr)))"), get_sub_expr_cnt);
  defun("sub-expr.expr", str2type("(fcn (ptr expr) (expr (ptr expr)) (idx u64))"), get_sub_expr);
  defun("sub-expr.skip", str2type("(fcn (ptr expr) (expr (ptr expr)) (idx u64))"), sub_expr_skip);
//expr * sub_expr_skip(expr * e)
  defun("make-sub-expr", str2type("(fcn (ptr expr) (exprs (ptr (ptr expr))) (cnt u64))"), make_sub_expr);
  defun("expr-symbol?", str2type("(fcn bool (expr (ptr expr)))"), is_expr_symbol);
    
  defun("expand-macro", str2type("(fcn (ptr expr) (ms (ptr macro_store)) (expr2 (ptr expr)))"), &expand_macro_store2);
  defun("print-macro-store", str2type("(fcn void (ms (ptr macro_store)))"), print_macro_store);
  
  defun("macro-store-args", str2type("(fcn i64 (ms (ptr macro_store)))"), macro_store_args);
  defun("gensym",str2type("(fcn (ptr expr))"), gensym);
  defun("free-expr", str2type("(fcn void (e (ptr expr)))"), free_expr);
}
