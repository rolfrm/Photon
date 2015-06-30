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
#include "lisp_types.h"
#include "lisp_parser.h"
#include "lisp_compiler.h"
#include "lisp_std_types.h"
#include "type_pool.h"
size_t get_sub_type_cnt(type_def * t){
  switch(t->type){
  case OPAQUE_STRUCT:
  case ENUM:
  case SIMPLE:
    return 0;
    break;
  case POINTER:
    return 1;
    break;
  case UNION:
  case STRUCT:
    return t->cstruct.cnt;
    break;
  case TYPEDEF:
    return 1;
    break;
  case FUNCTION:
    return 1 + t->fcn.cnt;
  case type_def_kind_cnt:
    ERROR("Unsupported type");
    break;
  }
  return 0;
}
	  
void get_sub_types(type_def * t, type_def ** out_types){
  switch(t->type){
  case OPAQUE_STRUCT:
  case ENUM:
  case SIMPLE:
    break;
  case POINTER:
    *out_types = t->ptr.inner;
    break;
  case UNION:
  case STRUCT:
    for(i64 i = 0; i < t->cstruct.cnt; i++){
      out_types[i] = t->cstruct.members[i].type;
    }
    break;
  case TYPEDEF:
    *out_types = t->ctypedef.inner;
    break;
  case FUNCTION:
    *out_types++ = t->fcn.ret;
    for(int i = 0; i < t->fcn.cnt; i++){
      *out_types++ = t->fcn.args[i];
    }
  case type_def_kind_cnt:
    ERROR("Unsupported type");
    break;
  }
}

type_def make_simple(char * name, size_t s){
  static type_def def;
  def.type = SIMPLE;
  def.simple.name = get_symbol(name);
  def.simple.size = s;
  return def;
}

type_def make_ptr(type_def * def){
  type_def out;
  out.type = POINTER;
  out.ptr.inner = def;
  return out;
}

void print_min_type(type_def * type){
  switch(type->type){
  case SIMPLE:
    format("%s", get_c_name(type->simple.name));
    break;
  case UNION:  
  case STRUCT:
  case OPAQUE_STRUCT:
    format("struct %s", get_c_name(type->cstruct.name));
    break;
  case POINTER:
    print_min_type(type->ptr.inner);
    format(" *");
    break;
  case ENUM:
    format("%s",get_c_name(type->cenum.name));
    break;
  case TYPEDEF:
   format("%s",get_c_name(type->ctypedef.name));
   break;
  case FUNCTION:
    ERROR("Cannot print function definition, only as decleration (named) ");
    break;
  case type_def_kind_cnt:
    ERROR("not implemented %i", type->type);
  }
}

type_def * get_fcn_ptr_function(type_def * td, int * _ptrs){
  int ptrs = 0;
  while(td->type == POINTER){td = td->ptr.inner; ptrs++;}
  if(td->type == FUNCTION){
    *_ptrs = ptrs;
    return td;
  }
  return NULL;
}

void print_function_decl(int ptrs, type_def * def, symbol name){
  ASSERT(def->type == FUNCTION);
  print_min_type(def->fcn.ret);
  format(" (%.*s %s)(",ptrs,"*",get_c_name(name));
  for(i64 i = 0; i < def->fcn.cnt; i++){
    int fptr_ptrs;
    type_def * fptr = get_fcn_ptr_function(def->fcn.args[i], &fptr_ptrs);
    if(fptr != NULL){
      char name[10];
      sprintf(name, "arg%i", i);
      print_function_decl(fptr_ptrs,fptr, get_symbol(name));
    }else print_min_type(def->fcn.args[i]);
    if(i + 1 != def->fcn.cnt)
      format(", ");
  }
  format(")");
}

void print_cdecl(decl idecl){
  
  type_def * def = idecl.type;
  switch(def->type){
  case ENUM:
  case UNION:
  case TYPEDEF:
  case STRUCT:
  case OPAQUE_STRUCT:
  case SIMPLE:
  case POINTER:
    {
      int inner_ptrs;
      type_def * fptr = get_fcn_ptr_function(def, &inner_ptrs);
      if(fptr != NULL){
	print_function_decl(inner_ptrs, fptr, idecl.name);
      }else{
	print_min_type(def);
	format(" %s",get_c_name(idecl.name));
      }
    }
    break;
  case FUNCTION:
    print_function_decl(0, def, idecl.name);
    break;
  case type_def_kind_cnt:
    ERROR("Not supported: '%i'\n", def->type);
  }
}

void print_decl(type_def * t, symbol name){
  decl dcl;
  dcl.name = name;
  dcl.type = t;
  print_cdecl(dcl);
}

void print_def(type_def * type){
  type_def * inner;
  switch(type->type){
  case SIMPLE:
    format("%s", get_c_name(type->simple.name));
    break;
  case OPAQUE_STRUCT:
    format("struct %s", get_c_name(type->cstruct.name));
    break;
  case STRUCT:
    format("struct %s{\n", type->cstruct.name.id == 0 ? "" : get_c_name(type->cstruct.name));
    for(i64 i = 0; i < type->cstruct.cnt; i++){	
      if(type->cstruct.members[i].name.id != 0){
	print_min_type(type->cstruct.members[i].type);
	format(" %s;\n",get_c_name(type->cstruct.members[i].name));
      }else{
	print_def(type->cstruct.members[i].type);
	format("\n");
      }
    }
    format("}"); 
    break;
  case POINTER:
    print_min_type(type->ptr.inner);
    format(" *");
    break;
  case ENUM:
    format("%s",symbol_name(type->cenum.name));
    break;
  case UNION:
    format("union {\n");
    for(i64 i = 0; i < type->cunion.cnt; i++){
      print_def(type->cunion.members[i].type);
      format(" %s;\n", symbol_name(type->cunion.members[i].name));
    }
    format("};");
    break;
 case TYPEDEF:
    inner = type->ctypedef.inner;
    format("typedef ");
    print_def(inner);
    format(" %s;\n", get_c_name(type->ctypedef.name));
    break;
  case FUNCTION:
    ERROR("Cannot print function definition, only as decleration (named) ");
    break;
  case type_def_kind_cnt:
    ERROR("not implemented %i", type->type);
  }
}

// if the code depends on *def it also depends on a number of other 
// types. This is however only what needs to be forward declared.
void _make_dependency_graph(type_def ** defs, type_def * def, bool nested, bool is_sizeless){
  bool check(){
    type_def ** defs_it = defs;
    for(; *defs_it != NULL; defs_it++){
      if(*defs_it == def)
	return false;
    }
    return true;
  }
  
  bool check_add(){
    type_def ** defs_it = defs;
    for(; *defs_it != NULL; defs_it++){
      if(*defs_it == def)
	return false;
    }
    *defs_it = def;
    return true;
  }
  
  if(def->type != TYPEDEF && check() == false) return;
  switch(def->type){
  case UNION:
    for(i64 i = 0; i < def->cunion.cnt; i++){
      type_def * sdef = def->cunion.members[i].type;
      _make_dependency_graph(defs,sdef,true, is_sizeless);
    }	  
    
    if(def->cunion.name.id != 0 && !nested) check_add();// *defs_it = def;
    break;
  case STRUCT:
    if(is_sizeless){
      _make_dependency_graph(defs, get_opaque(def),true, is_sizeless);
      return;
    }
    for(i64 i = 0; i < def->cstruct.cnt; i++){
      type_def * sdef = def->cstruct.members[i].type;
      _make_dependency_graph(defs,sdef,true, is_sizeless);
    }
    check_add();
    break;
  case POINTER:
    _make_dependency_graph(defs,def->ptr.inner,true, true);
    check_add();    
    break;
  case TYPEDEF:
    _make_dependency_graph(defs,def->ctypedef.inner, nested, is_sizeless);
    check_add();
    break;
    
  case FUNCTION:
    _make_dependency_graph(defs, def->fcn.ret, nested, is_sizeless);

    for(int i = 0; i < def->fcn.cnt; i++)
      _make_dependency_graph(defs, def->fcn.args[i],nested, is_sizeless);
    break;
  case OPAQUE_STRUCT:
  case ENUM:
    check_add();
    break;
  case SIMPLE:
    break;
  case type_def_kind_cnt:
    ERROR("Unknown type: %i",def->type);
    break;
  }
}


u64 size_of(type_def * t){
  size_t s = 0;
  switch(t->type){
  case UNION:
    for(int i = 0; i < t->cunion.cnt ; i++)
      s = MAX(size_of(t->cunion.members[i].type), s);
    return s;
  case STRUCT:
    for(int i = 0; i < t->cstruct.cnt ; i++){
      
	u64 thiss  = size_of(t->cstruct.members[i].type);
	u64 size_left = 0;//4 - (s % 4); // size_left in 4 byte align block.
	if(thiss > size_left){
	  s += size_left + thiss;
	}
	else{
	  s += thiss;
	}
	logd("size: %i %i %i\n",i, s, thiss);
      }
    return s;
  case FUNCTION:
  case POINTER:
  case ENUM:
    return sizeof(void *);
  case TYPEDEF:
    return size_of(t->ctypedef.inner);
  case OPAQUE_STRUCT:
    ERROR("cannot get size of opaque struct");
    break;
  case SIMPLE:
    return t->simple.size;
  case type_def_kind_cnt:
    ERROR("Unknown type");
    break;
  }

  return 0;
}



// if the code depends on *def it also depends on a number of other 
// types. This is however only what needs to be forward declared.
void make_dependency_graph(type_def ** defs, type_def * def){
  _make_dependency_graph(defs,def,false, false);
}

/*void get_no_size_dependencies(type_def ** defs, type_def * def){
  bool check(){
    type_def ** defs_it = defs;
    for(; *defs_it != NULL; defs_it++){
      if(*defs_it == def)
	return false;
    }
    return true;
  }
  
  bool check_add(){
    type_def ** defs_it = defs;
    for(; *defs_it != NULL; defs_it++){
      if(*defs_it == def)
	return false;
    }
    *defs_it = def;
    return true;
  }
  if(check() == false) return;
  switch(def->type){
  case UNION:
    check_add();
    break;
  case STRUCT:
    check_add();
    break;
  case POINTER:
    get_no_size_dependencies(defs, def->ptr.inner);
    break;
  case TYPEDEF:
    get_no_size_dependencies(defs, def->ctypedef.inner);
    check_add();    
    break;
    
  case FUNCTION:
    get_no_size_dependencies(defs, def->fcn.ret);
    for(int i = 0; i < def->fcn.cnt; i++)
      get_no_size_dependencies(defs, def->fcn.args[i]);
    break;
  case OPAQUE_STRUCT:
  case ENUM:
    check_add();
    break;
  case SIMPLE:
    break;
  case type_def_kind_cnt:
    ERROR("Unknown type: %i",def->type);
    break;
  }
  }*/

void write_dependencies(type_def ** deps){
  format("#include \"cstd_header.h\"\n");
  // first forward declare.

  for(; *deps != NULL; deps++){
    for(type_def ** deps2 = deps + 1; *deps2 != NULL; deps2++){
      ASSERT(*deps != *deps2);
    }
    type_def * t = *deps;
    
    if(t->type == STRUCT || t->type == OPAQUE_STRUCT){
      print_def(t);
      format(";\n");
    }
    if(t->type == ENUM){
      ERROR("Should not happen");
    }
    if(t->type == TYPEDEF){
      type_def * inner = t->ctypedef.inner;
      if(inner->type == ENUM){
	format("typedef enum {\n");
	for(int j = 0; j < inner->cenum.cnt; j++){
	  char * comma = (j !=(inner->cenum.cnt-1) ? "," : "");
	  format("   %s = %i%s\n", symbol_name(inner->cenum.names[j]), inner->cenum.values[j], comma);
	}
	format("}%s;\n", get_c_name(t->ctypedef.name));
      
      }else{
	//print_min_def(t);format(";\n");
	ASSERT(inner->type == STRUCT || inner->type == OPAQUE_STRUCT || (inner->type == POINTER && (inner->ptr.inner->type == STRUCT || inner->ptr.inner->type == OPAQUE_STRUCT)));
	decl dcl;
	dcl.name = t->ctypedef.name;
	dcl.type = inner;
	format("typedef ");
	print_cdecl(dcl);
	format(";\n");
      }
    }
  }
}

type_def * function_type(type_def * ret,size_t cnt, type_def ** ts){
  type_def td;
  td.type = FUNCTION;
  td.fcn.ret = ret;
  td.fcn.cnt = cnt;
  td.fcn.args = ts;
  return type_pool_get(&td);
}

// test //
bool test_print_c_code(){
  { // Simple include
    c_root_code c1;
    c1.type = C_INCLUDE_LIB;
    c1.include = "stdio.h";
    print_c_code(c1);
  }
  
  { // Complex, function definition
    c_value cv1a1;
    cv1a1.type = C_INLINE_VALUE;
    cv1a1.raw.value = "\"hello world!\"";
    cv1a1.raw.type = &char_ptr_def;

    c_value a_sym;
    a_sym.type = C_SYMBOL;
    a_sym.symbol = get_symbol("a");

    c_value cv1;
    cv1.type = C_FUNCTION_CALL;
    cv1.call.name = get_symbol("printf");

    cv1.call.arg_cnt = 1;
    cv1.call.args = &a_sym;

    c_expr expr;
    expr.type = C_VALUE;
    expr.value = cv1;
    
    c_fcndef fundef;
    type_def ftype;
    ftype.type = FUNCTION;
    ftype.fcn.cnt = 0;
    ftype.fcn.ret = &char_ptr_def;
    
    c_expr var;
    decl v;
    v.name = get_symbol("a");
    v.type = &char_ptr_def;
    var.type = C_VAR;
    var.var.var = v;
    var.var.value = &cv1a1;
    
    c_expr ret;
    ret.type = C_RETURN;
    ret.value = a_sym;

    c_expr exprs2[] = {var, expr, ret};
    
    c_block block;
    block.exprs = exprs2;
    block.expr_cnt = array_count(exprs2);
    
    c_expr expr3;
    expr3.type = C_BLOCK;
    expr3.block = block;
      
    fundef.block.exprs = &expr3;
    fundef.block.expr_cnt = 1;

    fundef.type = type_pool_get(&ftype);
    fundef.name = get_symbol("print_test");
    
    c_root_code c2;
    c2.type = C_FUNCTION_DEF;
    c2.fcndef = fundef;
    print_c_code(c2);
  }

  { // Complex type expansion
    c_root_code c3;
    c3.type = C_TYPE_DEF;
    c3.type_def = &type_def_def;
    print_c_code(c3);
  }

  return TEST_SUCCESS;
}
