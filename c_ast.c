#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <iron/types.h>
#include <iron/utils.h>
#include <iron/log.h>
#include <iron/test.h>
#include <iron/fileio.h>
#include <iron/mem.h>
#include <iron/array.h>
#include "lisp_parser.h"
#include "lisp_types.h"
#include "c_ast.h"
#include "lisp_compiler.h"
const c_block c_block_empty = {0,0};
const c_value c_value_empty = {0};
const c_expr c_expr_block = {.type = C_BLOCK};
const c_expr c_expr_value = {.type = C_VALUE};
void add_var_dep(symbol * vdeps, symbol newdep){
  ASSERT(symbol_name(newdep) != NULL);
  for(;!symbol_cmp(*vdeps, symbol_empty); vdeps++){
    if(symbol_cmp(*vdeps, newdep))
      return;
  }
  *vdeps = newdep;
}

#include "type_pool.h"
void value_dep(type_def ** deps, symbol * vdeps, c_value val){
  var_def * var;
  switch(val.type){
  case C_NOTHING:
    break;
  case C_INLINE_VALUE:
    make_dependency_graph(deps, val.raw.type);
    break;
  case C_FUNCTION_CALL:
    if(get_any_variable(val.call.name) != NULL)
      // function might be a local variable.
      add_var_dep(vdeps, val.call.name);
    make_dependency_graph(deps, val.call.type);
    for(size_t argi = 0; argi < val.call.arg_cnt; argi++){
      value_dep(deps, vdeps, val.call.args[argi]);
    }
    break;
  case C_OPERATOR:
    value_dep(deps, vdeps, *val.operator.left);
    value_dep(deps, vdeps, *val.operator.right);
    break;
  case C_DEREF:
    value_dep(deps, vdeps, *val.deref.inner);
    make_dependency_graph(deps, val.deref.return_type);
    break;
  case C_SUB_EXPR:
  case C_ADDRESS_OF:

    value_dep(deps, vdeps, *val.value);
    break;
  case C_SYMBOL:
    var = get_any_variable(val.symbol);
    if(var == NULL){
      // might be a local variable.
      return;
    }
    add_var_dep(vdeps, val.symbol);
    make_dependency_graph(deps, var->type);
    break;
  case C_MEMBER:
    make_dependency_graph(deps, val.member.type);
    value_dep(deps, vdeps, *val.member.item);
    break;
  case C_CAST:
    make_dependency_graph(deps, val.cast.type);
    value_dep(deps, vdeps, *val.cast.value);
  }
}

void expr_dep(type_def ** deps, symbol * vdeps, c_expr expr){
  switch(expr.type){
  case C_VAR:
    make_dependency_graph(deps, expr.var.var.type);
    if(expr.var.value != NULL)
      value_dep(deps, vdeps, *expr.var.value);
    break;
  case C_VALUE:
  case C_RETURN:
  case C_VALUE_UNENDED:
    value_dep(deps, vdeps, expr.value);
    break;
  case C_BLOCK:
    block_dep(deps, vdeps, expr.block);
    break;
  case C_KEYWORD:
    break;
  }
}

void block_dep(type_def ** deps, symbol * vdeps, c_block blk){
  for(size_t i = 0; i < blk.expr_cnt; i++){
    expr_dep(deps, vdeps, blk.exprs[i]);
  }
}

void c_root_code_dep(type_def ** deps, symbol * vdeps, c_root_code code){
  switch(code.type){
  case C_FUNCTION_DEF:
    make_dependency_graph(deps, code.fcndef.type);
    block_dep(deps, vdeps, code.fcndef.block);
    break;
  case C_VAR_DEF:
    make_dependency_graph(deps, code.var.var.type);
    if(code.var.value != NULL)
      value_dep(deps, vdeps, *code.var.value);
    break;
  case C_DECL:
    make_dependency_graph(deps, code.decl.type);
    break;
  case C_TYPE_DEF:
    make_dependency_graph(deps, code.type_def);
  case C_INCLUDE:
  case C_INCLUDE_LIB:
    break;
  }
}


void print_value(c_value val){
  switch(val.type){
  case C_ADDRESS_OF:
    format("&");
    print_value(*val.value);
    break;
  case C_MEMBER:
    format("(");
    print_value(*val.member.item);
    format(".%s)", get_c_name(val.member.name));;
    break;
  case C_DEREF:
    format("*");
    format("(");
    print_value(*val.deref.inner);
    format(")");
    break;
  case C_SUB_EXPR:
    format("(");
    print_value(*val.value);
    format(")");
    break;
  case C_INLINE_VALUE:
    format("%s", val.raw.value);
    break;
  case C_FUNCTION_CALL:
    {
      ASSERT(val.call.name.id != 0);
      /*var_def * fvar = get_any_variable(val.call.name);*/
      //ASSERT(fvar != NULL);
      //ASSERT(fvar->name.id == val.call.name.id);

      char * cname = get_c_name(val.call.name);
      char * lname = symbol_name(val.call.name);
      if(cname != lname){
	format(" /*%s*/ %s(",symbol_name(val.call.name), cname);
      }else{
	format("%s(",symbol_name(val.call.name), cname);
      }
    for(size_t i = 0; i < val.call.arg_cnt; i++){
      print_value(val.call.args[i]);

      if(i != val.call.arg_cnt -1){
	format(", ");
      }
    }
    format(")");
    }
    break;
  case C_OPERATOR:
    format("(");
    print_value(*val.operator.left);
    format(" %s ", val.operator.operator);
    print_value(*val.operator.right);
    format(")");
    break;
  case C_SYMBOL:
    format("%s", get_c_name(val.symbol));
    break;
  case C_CAST:
    format("((");
    print_decl(val.cast.type, get_symbol(NULL));
    format(")");
    print_value(*val.cast.value);
    format(")");
  case C_NOTHING:
    break;
  }
}

void print_c_var(c_var var){
  print_cdecl(var.var);
  if(var.value != NULL){
    format(" = ");
    print_value(*var.value);
  }
  format(";\n");
}

static void print_expr2(c_expr expr){
  switch(expr.type){
  case C_VAR:
    print_c_var(expr.var);
    break;
  case C_RETURN:
    format("return ");
  case C_VALUE_UNENDED:
  case C_VALUE:
    print_value(expr.value);
    if(expr.type != C_VALUE_UNENDED)
      format(";\n");
    break;
  case C_BLOCK:
    format("{\n");
    for(size_t i = 0; i < expr.block.expr_cnt; i++){
      print_expr2(expr.block.exprs[i]);
    }
    format("}\n");
    break;
  case C_KEYWORD:
    format("%s ", get_c_name(expr.keyword));
    break;
  }
}

void print_block(c_block blk){
  size_t var_cnt = 0;
  for(size_t i = 0; i < blk.expr_cnt; i++)
    if(blk.exprs[i].type == C_VAR) var_cnt++;  
  var_def _vars[var_cnt];
  var_def * vars = _vars;
  var_cnt = 0;

  format("{\n");
  push_symbols(&vars, &var_cnt);

  for(size_t i = 0; i < blk.expr_cnt; i++){
    if(blk.exprs[i].type == C_VAR){
      vars[var_cnt].name = blk.exprs[i].var.var.name;
      vars[var_cnt].type = blk.exprs[i].var.var.type;
      vars[var_cnt].data = &blk.exprs[i].var.value;
      var_cnt += 1;
    }
    print_expr2(blk.exprs[i]);
  }
  pop_symbols();
  format("}\n");
}

void print_fcn_code(c_fcndef fcndef){
  type_def * typeid  = fcndef.type;
  ASSERT(typeid->type == FUNCTION);
  print_min_type(typeid->fcn.ret);
  format(" %s(",get_c_name(fcndef.name));
  // ** handle variables ** //
  
  size_t varcnt = typeid->fcn.cnt;
  var_def _vars[varcnt];
  var_def * vars = _vars;
  for(size_t i = 0; i < varcnt; i++){
    type_def * arg_type = typeid->fcn.args[i];
    vars[i].data = NULL;
    vars[i].name = fcndef.args[i];
    vars[i].type = arg_type;
    print_decl(arg_type, vars[i].name);
    if(i != varcnt-1){
      format(",");
    }
  }
  format(")");
  push_symbols(&vars, &varcnt);
  print_block(fcndef.block);
  pop_symbols();
}

void print_c_code(c_root_code code){
  switch(code.type){
  case C_INCLUDE:
    format("#include \"%s\"\n",code.include);
    break;
  case C_INCLUDE_LIB:
    format("#include <%s>\n",code.include);
    break;
  case C_FUNCTION_DEF:

    print_fcn_code(code.fcndef);
    break;
  case C_VAR_DEF:
    print_c_var(code.var);
    break;
  case C_TYPE_DEF:
    print_def(code.type_def);format(";\n");
    break;
  case C_DECL:
    print_cdecl(code.decl);
    format(";\n");
    break;
  }
}

void block_add(c_block * blk, c_expr expr){
  list_add((void **) &blk->exprs, &blk->expr_cnt, &expr, sizeof(c_expr));
}

void c_var_delete(c_var var){
  if(var.value != NULL){
    c_value_delete(*var.value);
    dealloc(var.value);
  }
}

void c_expr_delete(c_expr expr){
  switch(expr.type){
  case C_VAR:
    c_var_delete(expr.var);
    break;
  case C_RETURN:
  case C_VALUE_UNENDED:
  case C_VALUE:
    c_value_delete(expr.value);
    break;
  case C_BLOCK:
    c_block_delete(expr.block);
    break;
  case C_KEYWORD:
    break;
  }
}

void c_block_delete(c_block blk){
  for(size_t i = 0; i < blk.expr_cnt; i++){
    c_expr_delete(blk.exprs[i]);
  }
  list_clean((void **) &blk.exprs, &blk.expr_cnt);
}

void c_value_delete(c_value val){
  switch(val.type){
  case C_ADDRESS_OF: 
  case C_SUB_EXPR:
    c_value_delete(*val.value);
    dealloc(val.value);
    break;
  case C_INLINE_VALUE:
    //dealloc(val.raw.value);
    break;
  case C_FUNCTION_CALL: 
    for(size_t i = 0; i < val.call.arg_cnt; i++){
      c_value_delete(val.call.args[i]);
    }
    dealloc(val.call.args);
    break;
  case C_OPERATOR: 
    c_value_delete(*val.operator.left);
    c_value_delete(*val.operator.right);
    dealloc(val.operator.left);
    dealloc(val.operator.right);
    break;
  case C_DEREF: 
    c_value_delete(*val.deref.inner);
    dealloc(val.deref.inner);
    break;
    
  case C_SYMBOL: break;
  case C_CAST: 
    c_value_delete(*val.cast.value);
    break;
  case C_NOTHING: break;
  case C_MEMBER: 
    c_value_delete(*val.member.item);
    break;
  }
}

void c_root_code_delete(c_root_code code){
  switch(code.type){
  case C_FUNCTION_DEF: 
    c_block_delete(code.fcndef.block);
    break;
    
  case C_VAR_DEF: 
    c_var_delete(code.var);
    break;
  case C_INCLUDE: 
  case C_INCLUDE_LIB: 
    dealloc(code.include);
    break;
  case C_DECL: break;
  case C_TYPE_DEF:break;
  }
}

c_value c_value_sub_expr(c_value * val){
  c_value _val = {.type = C_SUB_EXPR, .value = val};
  return _val;
}

c_expr c_expr_keyword(char * keyword){
  c_expr expr;
  expr.type = C_KEYWORD;
  expr.keyword = get_symbol(keyword);
  return expr;
}
