#include <stdio.h>
#include <iron/full.h>
#include "lisp_parser.h"
#include <libtcc.h>
#include "lisp_types.h"
#include "lisp_std_types.h"
#include "lisp_compiler.h"
#include "type_pool.h"

#define COMPILE_ASSERT(expr) if(!(expr)){ERROR("Compile error '" #expr "'");return &error_def;}
#define COMPILE_ERROR(fmt, ...) {ERROR(fmt,##__VA_ARGS__); return &error_def;}

symbol vexpr_symbol(value_expr e){
  char buf[e.strln + 1];
  buf[e.strln] = 0;
  memcpy(buf, e.value, e.strln);
  return get_symbol(buf);
}

symbol expr_symbol(expr e){
  return vexpr_symbol(e.value);
}

expr symbol_expr(char * name){
  expr e;
  e.type = VALUE;
  e.value.type = SYMBOL;
  e.value.value = name;
  e.value.strln = strlen(name);
  return e;
}

expr symbol_expr2(symbol name){
  return symbol_expr(symbol_name(name));
}

expr string_expr(char * name){
  expr e = symbol_expr(name);
  e.value.type = STRING;
  return e;
}

bool is_symbol(expr exp){
  return exp.type == VALUE && exp.value.type == SYMBOL;
}

bool is_string(expr exp){
  return exp.type == VALUE && exp.value.type == STRING;
}

bool is_keyword(expr exp){
  return exp.type == VALUE && exp.value.type == KEYWORD;
}

char * read_symbol(expr name){
  return fmtstr("%.*s",name.value.strln, name.value.value);
}
	
typedef struct{
  c_root_code * c_code;
  size_t code_cnt;
}compiled_lisp;

static type_def * compile_value(c_value * val, value_expr e){
  val->type = C_INLINE_VALUE;
  var_def * vdef;
  switch(e.type){
  case STRING:
    val->raw.value = fmtstr("\"%.*s\"",e.strln,e.value);
    val->raw.type = &char_ptr_def;
    return type_pool_get(&char_ptr_def);
  case KEYWORD:
  case SYMBOL:
    val->type = C_SYMBOL;
    val->symbol = vexpr_symbol(e);
    vdef = get_variable(val->symbol);
    if(vdef == NULL){
      COMPILE_ERROR("Unknown variable '%s'", symbol_name(val->symbol));
    }
    return type_pool_get(vdef->type);
  case NUMBER:
    val->raw.value = fmtstr("%.*s",e.strln,e.value);
    val->raw.type = &i64_def;
    return type_pool_get(&i64_def);
    break;
  default:
    break;
  }
  logd("type: %i\n", e.type);
  COMPILE_ERROR(false);
}

type_def * _type_macro(expr typexpr);
bool read_decl(expr dclexpr, decl * out){
  if(dclexpr.type == EXPR){
    sub_expr sexpr = dclexpr.sub_expr;
    if(sexpr.cnt == 2){
      expr name = sexpr.exprs[0];
      expr type = sexpr.exprs[1];
      if(name.type == VALUE && name.value.type == SYMBOL){
	out->name = expr_symbol(name);
	out->type = _type_macro(type);
	return &error_def != out->type;
      }
    }
  }
  return false;
}

type_def * _type_macro(expr typexpr){
  if(typexpr.type == EXPR){
    sub_expr sexp = typexpr.sub_expr;
    COMPILE_ASSERT(sexp.cnt > 0);
    expr kind = sexp.exprs[0];
    COMPILE_ASSERT(kind.type == VALUE && kind.value.type == SYMBOL);
    value_expr vkind = kind.value;

    if(strncmp(vkind.value,"fcn",vkind.strln) == 0){
      type_def out;
      out.type = FUNCTION;
      COMPILE_ASSERT(sexp.cnt > 1);
      type_def * ret = _type_macro(sexp.exprs[1]);
    
      COMPILE_ASSERT(&error_def != ret);
      decl args[sexp.cnt - 2];
      for(size_t i = 0; i < sexp.cnt - 2; i++){
	COMPILE_ASSERT(read_decl(sexp.exprs[i + 2], args + i));
      } 
      out.fcn.ret = ret;
      out.fcn.args = clone(args, sizeof(args));
      out.fcn.cnt = array_count(args);
      return type_pool_get(&out);

    }else if (strncmp(vkind.value,"ptr",vkind.strln) == 0){
      COMPILE_ASSERT(sexp.cnt == 2);
      type_def out;
      out.type = POINTER;
      out.ptr.inner = _type_macro(sexp.exprs[1]);
      return type_pool_get(&out);
    }else if (strncmp(vkind.value, "struct",vkind.strln) == 0){
      COMPILE_ASSERT(sexp.cnt >= 2);
      symbol name = symbol_empty;
      bool is_anon = sexp.exprs[1].type == EXPR;
      if(!is_anon){
	COMPILE_ASSERT(sexp.exprs[1].value.type == SYMBOL);
	name = vexpr_symbol(sexp.exprs[1].value);
      }
      size_t memcnt = sexp.cnt - 1 - (is_anon ? 0 : 1);
      expr * sub = sexp.exprs + 1 + (is_anon ? 0 : 1);
      decl members[memcnt];
      for(size_t i = 0 ; i < memcnt; i++){
	COMPILE_ASSERT(sub->type == EXPR && sub->sub_expr.cnt == 2);
	sub_expr sx = sub->sub_expr;
	COMPILE_ASSERT(sx.exprs[0].type == VALUE && sx.exprs[0].value.type == SYMBOL);
	members[i].name = vexpr_symbol(sx.exprs[0].value);
	members[i].type = _type_macro(sx.exprs[1]);
      }
      type_def out;
      out.type = STRUCT;
      out.cstruct.members = clone(members,sizeof(members));
      out.cstruct.cnt = memcnt;
      out.cstruct.name = name;
      return type_pool_get(&out);
    }else if(strncmp(vkind.value, "opaque-struct", vkind.strln) == 0){
      COMPILE_ASSERT(sexp.cnt == 2);
      type_def out;
      out.type = OPAQUE_STRUCT;
      out.cstruct.members = NULL;
      out.cstruct.cnt = 0;
      out.cstruct.name = vexpr_symbol(sexp.exprs[1].value);
      return type_pool_get(&out);
    }else if (strncmp(vkind.value, "alias", vkind.strln) == 0){
      COMPILE_ASSERT(sexp.cnt == 3);
      COMPILE_ASSERT(is_symbol(sexp.exprs[2]));
      type_def out;
      out.type = TYPEDEF;
      out.ctypedef.name = vexpr_symbol(sexp.exprs[2].value);
      out.ctypedef.inner = _type_macro(sexp.exprs[1]);
      return type_pool_get(&out);
    }
  }else{
    type_def * td = type_pool_simple(expr_symbol(typexpr));
    td = type_pool_get(td);
    COMPILE_ASSERT(td != NULL);
    return td;
  }
  return &error_def;
}
  
static type_def * _compile_expr(c_block * block, c_value * val,  expr e );

static type_def * __compile_expr(c_block * block, c_value * value, sub_expr * se){
  UNUSED(value);
  if(se->cnt == 0)
    ERROR("sub expressio count 0");
  expr name_expr = se->exprs[0];
  if(name_expr.type != VALUE && name_expr.value.type != SYMBOL) ERROR("need symbol for first car");
  
  expr * args = se->exprs + 1;
  i64 argcnt = se->cnt - 1;

  symbol name = expr_symbol(name_expr);

  var_def * fvar = get_variable(name);
  if(fvar == NULL) COMPILE_ERROR("unknown symbol '%s'", symbol_name(name));
  if(fvar->type == type_pool_get(&cmacro_def_def)){
    cmacro_def * macro = fvar->data;

    if(macro->arg_cnt != argcnt && macro->arg_cnt != -1)
      ERROR("Unsupported number of arguments %i for %s",argcnt, macro->name);

    switch(macro->arg_cnt){
    case 0:
      return ((type_def *(*)(c_block * block, c_value * value)) macro->fcn)(block,value);
    case 1:
      return ((type_def *(*)(c_block * block, c_value * value,expr)) macro->fcn)(block,value,args[0]);
    case 2:
      return ((type_def *(*)(c_block * block, c_value * value,expr,expr)) 
	      macro->fcn)(block,value,args[0],args[1]);
    case 3:
      return ((type_def *(*)(c_block * block, c_value * value, expr,expr,expr)) 
	      macro->fcn)(block,value,args[0],args[1],args[2]);
    case -1:

      return ((type_def *(*)(c_block * block, c_value * value, expr *,size_t))macro->fcn)(block,value,args, argcnt);
    default:
      ERROR("Number of macro arguments not supported: %i", argcnt);
    }
  }else if(fvar->type->type == FUNCTION){
    type_def * td = fvar->type;
    COMPILE_ASSERT(td->fcn.cnt == argcnt);

    c_value fargs[argcnt];
    type_def * farg_types[argcnt];
    for(i64 i = 0; i < argcnt; i++){
      farg_types[i] = _compile_expr(block, fargs + i, args[i]);
      if(type_pool_get(farg_types[i]) != type_pool_get(td->fcn.args[i].type)){
	logd("ERROR: got '");
	print_def(farg_types[i],true);
	logd("' expected '");
	print_def(td->fcn.args[i].type,true);
	logd("'\n");
	ERROR("Non matching types");
      }
    }
    
    c_function_call call;
    call.type = td;
    call.name = fvar->name;
    
    call.args = clone(fargs,sizeof(fargs));
    value->type = C_FUNCTION_CALL;
    value->call = call;
    value->call.arg_cnt = argcnt;
    
    return td->fcn.ret;
  }else{
    ERROR("Not supported.. %i\n", fvar->type->type);
  }
  type_def * sub_types[se->cnt];
  UNUSED(sub_types);
  c_value val[se->cnt];
  for(size_t i = 0; i < se->cnt; i++){
    expr * e = se->exprs + i;
    sub_types[i] = _compile_expr(block, val + i, *e);
  }
  return &error_def;
}
	  
static type_def * _compile_expr(c_block * block, c_value * val,  expr e ){
  type_def * td;
  switch(e.type){
  case EXPR:
    return __compile_expr(block, val, &e.sub_expr);
    break;
  case VALUE:
    td = compile_value(val,e.value);
    break;
  }	  
  return td;
}

c_root_code compile_lisp_to_eval(expr exp){
  c_root_code r;
  c_fcndef * f = &r.fcndef;
  r.type = C_FUNCTION_DEF;

  f->block.expr_cnt = 0;
  f->block.exprs = NULL; 
  c_value val;
  type_def * t = _compile_expr(&f->block, &val, exp);
  type_def td;
  td.type = FUNCTION;
  td.fcn.ret = t;
  td.fcn.args = NULL;
  td.fcn.cnt = 0;

  f->fdecl.name = get_symbol("eval");
  f->fdecl.type = type_pool_get(&td);

  c_expr expr;
  expr.type = C_VALUE;
  if(t != type_pool_get(&void_def)) expr.type = C_RETURN;
  expr.value = val;
  list_add((void **) &f->block.exprs, &f->block.expr_cnt, &expr, sizeof(c_expr));

  return r;
}

type_def * str2type(char * str){
  return _type_macro(lisp_parse1(str));
}

void print_type(type_def * def){
  logd("type: '")print_def(def,true); logd("'\n");
}
void write_line(char * str){
  logd("%s\n", str);
}

#include <libtcc.h>
#include <stdlib.h>
void tccerror(void * opaque, const char * msg){
  UNUSED(opaque);
  format("%s\n",msg);
}

TCCState * mktccs(){
  TCCState * tccs = tcc_new();
  tcc_set_lib_path(tccs,".");
  tcc_set_error_func(tccs, NULL, tccerror);
  tcc_set_output_type(tccs, TCC_OUTPUT_MEMORY);
  return tccs;
}

void go_write(type_def ** deps, symbol * vdeps, c_root_code * codes, size_t code_cnt){

  write_dependencies(deps);
  for(size_t i = 0; deps[i] != NULL; i++){
    if(deps[i]->type == TYPEDEF){
      continue;
      print_def(deps[i]->ctypedef.inner,false);
    }else{
      print_def(deps[i],false);
    }
    format(";\n");
  }
    
  for(size_t i = 0; vdeps[i].id != 0; i++){
      
    var_def * var = get_variable(vdeps[i]);
    ASSERT(var != NULL);
    decl dcl;
    dcl.name = var->name;
    dcl.type = var->type;
    format("extern ");
    print_cdecl(dcl);format(";\n");
  }
  
  for(size_t i = 0; i < code_cnt; i++){
    c_root_code_dep(deps, vdeps, codes[i]);
    print_c_code(codes[i]);
  }
}

void compile_as_c(c_root_code * codes, size_t code_cnt){
  type_def * deps[100];
  symbol vdeps[100];
  memset(deps, 0, sizeof(deps));
  memset(vdeps, 0, sizeof(vdeps));
  for(size_t i = 0; i < code_cnt; i++){
    c_root_code_dep(deps, vdeps, codes[i]);
  }
  char * data = NULL;
  size_t cnt = 0;
  FILE * f = open_memstream(&data, &cnt);
  push_format_out(f);
  go_write(deps, vdeps, codes, code_cnt);
  pop_format_out();
  fclose(f);

  char header[] = "//***********\n";
  append_buffer_to_file(header,sizeof(header) - 1,"compile_out.c");
  append_buffer_to_file(data,cnt,"compile_out.c");
  TCCState * tccs = mktccs();
  for(size_t i = 0; i < array_count(vdeps) && vdeps[i].id != 0; i++){

    var_def * var = get_variable(vdeps[i]);
    if(var->type->type == FUNCTION){
      int fail = tcc_add_symbol(tccs,get_c_name(var->name),var->data);
      ASSERT(!fail);
    }else{
      char * vname = get_c_name(var->name);
      int fail = tcc_add_symbol(tccs,vname,var->data);
      ASSERT(!fail);
    }
  }
  
  int fail = tcc_compile_string(tccs, data);
  free(data);
  ASSERT(!fail);
  data = NULL;
  int size = tcc_relocate(tccs, NULL);
  fail = tcc_relocate(tccs, alloc(size));
  ASSERT(!fail);
  
  for(size_t i = 0; i < code_cnt; i++){
    c_root_code r = codes[i];
    if(r.type == C_FUNCTION_DEF){
      decl fdecl = r.fcndef.fdecl;
      void * ptr = tcc_get_symbol(tccs, get_c_name(fdecl.name));
      ASSERT(ptr != NULL);
      compiler_define_variable_ptr(fdecl.name, fdecl.type, ptr);
    }else if(r.type == C_VAR_DEF){

      decl vdecl = r.var.var;
      void * ptr = tcc_get_symbol(tccs, get_c_name(vdecl.name));
      ASSERT(ptr != NULL);
      compiler_define_variable_ptr(vdecl.name, vdecl.type, ptr);
    }
  }

  tcc_delete(tccs);
}

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
	  logd("got '"); print_def(td,false); logd("' expected '");
	  print_def(exprtd,false); logd("'\n");
	  ERROR("Types does not match");
	}
      }));
  c_expr expr;
  expr.value = _val;
  expr.type = C_RETURN;
  blk->exprs = &expr;
  blk->expr_cnt = 1;
  
  decl argsdecl[argcnt];
  for(size_t i = 0; i < argcnt; i++){
    argsdecl[i].type = exprtd;
    argsdecl[i].name = _vars[i].name;
  }
  type_def * fcnt = function_type(exprtd, argcnt, argsdecl);

  decl *fdecl = &f->fdecl;
  fdecl->name = name;
  fdecl->type = fcnt;

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
  decl *fdecl = &f->fdecl;
  fdecl->name = fcnname;
  
  expr subexpr[args.sub_expr.cnt + 1];
  subexpr[0] = symbol_expr("fcn");
  for(size_t i = 1; i < args.sub_expr.cnt + 1; i++){
    subexpr[i] = args.sub_expr.exprs[i-1];
  }
  
  expr typexpr = mk_sub_expr(subexpr, array_count(subexpr));

  type_def * typeid = _type_macro(typexpr);
  fdecl->type = typeid;
  // ** register arguments as symbols ** //
  size_t varcnt = typeid->fcn.cnt;
  var_def _vars[typeid->fcn.cnt];
  var_def * vars = _vars;
  for(size_t i = 0; i < varcnt; i++){
    vars[i].data = NULL;
    vars[i].name = typeid->fcn.args[i].name;
    vars[i].type = typeid->fcn.args[i].type;
  }
  
  // ** Compile body with symbols registered ** //
  c_value val;
  with_symbols(&vars, &varcnt, lambda(void, (){
	type_def * td = _compile_expr(blk,&val, body);
	ASSERT(td == typeid->fcn.ret);
      }));
  c_expr expr;
  if(typeid->fcn.ret == &void_def){
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

i64 i64_add(i64 a, i64 b){
  return a + b;
}

double double_add(double a, double b){ return a + b;}
double double_sub(double a, double b){ return a - b;}
double double_div(double a, double b){ return a / b;}
double double_mul(double a, double b){ return a * b;}
void defun(char * name, type_def * t, void * fcn){
  compiler_define_variable_ptr(get_symbol(name), t, fcn);
}

void lisp_load_compiler(compiler_state * c){
  with_compiler(c, lambda(void, (){
	
	// Types
	load_defs();

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

	// Functions
	type_def * type = str2type("(fcn void (a (ptr type_def)))");
	compiler_define_variable_ptr(get_symbol("print_type"), type, print_type);
	compiler_define_variable_ptr(get_symbol("write_line"), 
				     str2type("(fcn void (a (ptr char)))"), &write_line);
	compiler_define_variable_ptr(get_symbol("i64+"), 
				     str2type("(fcn i64 (a i64) (b i64))"), &i64_add);
	
	compiler_define_variable_ptr(get_symbol("get-symbol"), 
				     str2type("(fcn (ptr symbol) (a (ptr char)))"), get_symbol2);
	type_def * d2t =  str2type("(fcn f64 (a f64) (b f64))");
	defun("f+", d2t, double_add);
	defun("f-", d2t, double_sub);
	defun("f/", d2t, double_div);
	defun("f*", d2t, double_mul);
	
      }));
}

var_def * lisp_compile_expr(expr ex){
  c_root_code cl = compile_lisp_to_eval(ex);
  if(cl.fcndef.fdecl.type->fcn.ret == &error_def)
    return NULL;
  compile_as_c(&cl,1);
  symbol s = get_symbol("eval");
  return get_variable(s);
}

void * lisp_compile_and_run_expr(expr ex){
  var_def * var = lisp_compile_expr(ex);
  void * (*fcn)() = var->data;
  return fcn();
}


void lisp_run_expr(expr ex){

  var_def * evaldef = lisp_compile_expr(ex);
  ASSERT(evaldef != NULL);
  print_def(evaldef->type->fcn.ret,false); logd(" :: ");
  type_def * ret = evaldef->type->fcn.ret;
  if(ret == &void_def){
    logd("()\n");
    void (* fcn)() = evaldef->data;
    fcn();
  }else if(ret == str2type("(ptr type_def)")){
    type_def * (* fcn)() = evaldef->data;
    fcn();
    logd("type\n");
  }else if(ret == &char_ptr_def){
    char * (* fcn)() = evaldef->data;
    char * str = fcn();
    logd("\"%s\"\n",str);
  }else if(ret == str2type("(ptr symbol)")){
    symbol * (* fcn)() = evaldef->data;
    symbol * s = fcn();
    logd("'%s\n", symbol_name(*s));
    
  }else if(ret->type == POINTER || ret->type == FUNCTION){
    void * (* fcn)() = evaldef->data;
    void * ptr = fcn();
    logd("%p\n", ptr);
  }else if(ret == &f32_def){
    f32 (* fcn)() = evaldef->data;
    f32 v = fcn();
    logd("%f\n",v);  
  }else if(ret == &f64_def){
    f64 (* fcn)() = evaldef->data;
    f64 v = fcn();
    logd("%f\n",v);  
  }else if(ret->type == SIMPLE){
    i64 (* fcn)() = evaldef->data;
    i64 v = fcn();
    logd("%i\n",v);
  }else if(ret == &error_def){
    
  }else{
    void * (* fcn)() = evaldef->data;
    void * v = fcn();
    logd("try %p\n", v);
  }
}

void lisp_run_exprs(compiler_state * c, expr * exprs, size_t exprcnt){
  lisp_load_compiler(c);
  with_compiler(c,lambda(void, (){
	for(size_t i = 0; i < exprcnt; i++){
	  lisp_run_expr(exprs[i]);
	}};));
}



void lisp_run_script_file(char * filepath){
  char * code = read_file_to_string(filepath);
  lisp_run_script_string(code);
  dealloc(code);
}

void lisp_run_script_string(char * code){
  size_t exprcnt;
  expr * exprs = lisp_parse_all(code, &exprcnt);
  lisp_run_exprs(get_compiler(), exprs, exprcnt);
}
bool test_tcc();
bool test_lisp2c(){
  TEST(test_tcc);
  load_defs();
  compiler_state * c = compiler_make();
  lisp_load_compiler(c);
  bool ret = TEST_SUCCESS;
  with_compiler(c,lambda(void, (){
	type_def * type = str2type("(fcn void (a (ptr type_def)))");
	type_def * type2 = str2type("(fcn void (a (ptr type_def)))");
	type_def * type3 = str2type("(fcn void (a (ptr void)))");
	ASSERT(type == type2 && type != type3);
	
	type_def * d = str2type("(alias (ptr type_def) td)");
	print_def(d,false);
	type_def * d2 = str2type("(alias (struct _vec2 (x f32) (y f32)) vec2)");
	print_def(d2,false);
	type_def * d3 = str2type("(ptr vec2)");
	print_def(d3,false);	
	type_def * d4 = str2type("(alias (struct _a) a)");
	print_def(d4, false);
      }));
  return ret;
}

bool test_tcc(){
  char * code = "extern int * item; int * eval(){ return item;}";
  TCCState * tccs = mktccs();
  int _data2 = 123;
  int * data1 = clone(&_data2,sizeof(int));;
  int fail = tcc_add_symbol(tccs,"item",&data1);
  ASSERT(!fail);
  fail = tcc_compile_string(tccs, code);
  ASSERT(!fail);
  int size = tcc_relocate(tccs, NULL);
  fail = tcc_relocate(tccs, alloc(size));
  ASSERT(!fail);
  int * (* eval)() = tcc_get_symbol(tccs, "eval");
  tcc_delete(tccs);
  TEST_ASSERT(data1 == eval());
  return TEST_SUCCESS;
}
