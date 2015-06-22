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
#include "lisp_compiler.h"
const c_block c_block_empty = {0,0};

void add_var_dep(symbol * vdeps, symbol newdep){
  for(;!symbol_cmp(*vdeps, symbol_empty); vdeps++){
    if(symbol_cmp(*vdeps, newdep))
      return;
  }
  *vdeps = newdep;
}

void value_dep(type_def ** deps, symbol * vdeps, c_value val){
  var_def * var;
  switch(val.type){
  case C_NOTHING:
    break;
  case C_INLINE_VALUE:
    make_dependency_graph(deps, val.raw.type);
    break;
  case C_FUNCTION_CALL:
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
  case C_SUB_EXPR:
  case C_DEREF:
  case C_ADDRESS_OF:
    value_dep(deps, vdeps, *val.value);
    break;
  case C_SYMBOL:
    var = get_variable(val.symbol);
    if(var == NULL){
      //ERROR("Undefined symbol '%s'",val.symbol);
      // might be a local variable.
      return;
    }
    add_var_dep(vdeps, val.symbol);
    make_dependency_graph(deps, var->type);
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
    if(expr.var.value != NULL){
      //add_var_dep(vdeps, expr.var.var.name);
      value_dep(deps, vdeps, *expr.var.value);
    }

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
    //add_var_dep(vdeps, code.var.var.name);
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
  case C_DEREF:
    format("*");
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
    print_min_type(val.cast.type);
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
  with_symbols(&vars, &var_cnt, lambda(void, (){
	for(size_t i = 0; i < blk.expr_cnt; i++){
	  if(blk.exprs[i].type == C_VAR){
	    vars[var_cnt].name = blk.exprs[i].var.var.name;
	    vars[var_cnt].type = blk.exprs[i].var.var.type;
	    vars[var_cnt].data = &blk.exprs[i].var.value;
	    var_cnt += 1;
	  }
	  print_expr2(blk.exprs[i]);
	}
      }));
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
    print_min_type(arg_type);
    format(" %s", get_c_name(vars[i].name));
    if(i != varcnt-1){
      format(",");
    }
  }
  format(")");
  with_symbols(&vars,&varcnt,lambda(void,(){print_block(fcndef.block);}));
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
