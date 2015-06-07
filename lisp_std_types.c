#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <iron/types.h>
#include <iron/log.h>
#include <iron/mem.h>
#include "lisp_types.h"

#include "lisp_std_types.h"
#include "type_pool.h"
static void r(type_def * def){
  type_pool_reg_static(def);
}

void r2(type_def * def){
  type_pool_reg_static(def);
}

// Loads a all the types we need.
// call before anything else.
void load_defs(){
  // simple types //
  void_def = make_simple("void");
  r(&void_def);
  void_ptr_def = make_ptr(&void_def);
  r2(&void_ptr_def);
  error_def = make_simple("error");
  r2(&error_def);
  char_def = make_simple("char");
  r2(&char_def);
  i64_def = make_simple("i64");
  r2(&i64_def);
  i32_def = make_simple("i32");
  r2(&i32_def);
  i16_def = make_simple("i16");
  r2(&i16_def);
  i8_def = make_simple("i8");
  r2(&i8_def);
  u64_def = make_simple("u64");
  r2(&u64_def);
  u32_def = make_simple("u32");
  r2(&u32_def);
  u16_def = make_simple("u16");
  r2(&u16_def);
  u8_def = make_simple("u8");
  r2(&u8_def);
  f32_def = make_simple("f32");
  r2(&f32_def);
  f64_def = make_simple("f64");
  r2(&f64_def);
  // pointers to simple types //
  char_ptr_def.type = POINTER;
  char_ptr_def.ptr.inner = &char_def;
  r2(&char_ptr_def);
  char_ptr_ptr_def.type = POINTER;
  char_ptr_ptr_def.ptr.inner = &char_ptr_def;
  r2(&char_ptr_ptr_def);
  i64_ptr_def.type = POINTER;
  i64_ptr_def.ptr.inner = &i64_def;  
  r2(&i64_ptr_def);

  { // struct symbol
    symbol_def.type = TYPEDEF;
    symbol_def.ctypedef.name = get_symbol("symbol");
    
    static decl members[1];
    static type_def inner;
    symbol_def.ctypedef.inner = &inner;
    inner.type = STRUCT;
    members[0].type = &u64_def;
    members[0].name = get_symbol("id");
    inner.cstruct.members = members;
    inner.cstruct.cnt = array_count(members);
    inner.cstruct.name = get_symbol("_symbol_");
    r2(&symbol_def);
  }


  type_def_def.type = TYPEDEF;
  type_def_def.ctypedef.name = get_symbol("type_def");
  type_def_ptr_def.type = POINTER;
  type_def_ptr_def.ptr.inner = &type_def_def;  
  decl_ptr_def.type = POINTER;
  decl_ptr_def.ptr.inner = &decl_def;
  
  { // kind enum
    static type_def type_def_kind_def_inner;
    type_def_kind_def.type = TYPEDEF;
    type_def_kind_def.ctypedef.inner = &type_def_kind_def_inner;
    type_def_kind_def.ctypedef.name = get_symbol("type_def_kind");
    static char * kindnames[] = {"SIMPLE", "FUNCTION", "POINTER", "STRUCT", "UNION", "ENUM"};
    static symbol kindnames2[array_count(kindnames)];
    for(size_t i = 0; i < array_count(kindnames); i++)
      kindnames2[i] = get_symbol(kindnames[i]);
    static i64 kindvalues[] = {SIMPLE, FUNCTION, POINTER, STRUCT, UNION, ENUM};
    type_def_kind_def_inner.type = ENUM;
    type_def_kind_def_inner.cenum.cnt = array_count(kindnames);
    type_def_kind_def_inner.cenum.names = kindnames2;
    type_def_kind_def_inner.cenum.values = kindvalues;
    r2(&type_def_kind_def);
  }
    
  { //type_def struct members:
    static type_def itype_def_def;
    type_def_def.ctypedef.inner = &itype_def_def;
    
    static decl members[2];
    itype_def_def.type = STRUCT;
    itype_def_def.cstruct.members = members;
    itype_def_def.cstruct.cnt = array_count(members);
    itype_def_def.cstruct.name = get_symbol("_type_def");
    
    members[0].type = &type_def_kind_def;
    members[0].name = get_symbol("kind");
    {
      static type_def type_def_union;
      static decl umembers[7];
      type_def_union.type = UNION;    
      type_def_union.cunion.name = symbol_empty;
      type_def_union.cunion.cnt = array_count(umembers);
      type_def_union.cunion.members = umembers;
      
      members[1].type = &type_def_union;

      members[1].name = symbol_empty;

      {// anon union members
	
	{//cenum
	  static type_def cenum_def;
	  static decl cenum_members[4];
	  cenum_members[0].name = get_symbol("names");
	  cenum_members[0].type = &char_ptr_ptr_def;
	  cenum_members[1].name = get_symbol("values");
	  cenum_members[1].type = &i64_ptr_def;
	  cenum_members[2].name = get_symbol("cnt");
	  cenum_members[2].type = &i64_def;
	  cenum_members[3].name = get_symbol("name");
	  cenum_members[3].type = &char_ptr_def;
	  
	  cenum_def.type = STRUCT;
	  cenum_def.cstruct.members = cenum_members;
	  cenum_def.cstruct.name = get_symbol("_enum");
	  cenum_def.cstruct.cnt = array_count(cenum_members);
	  umembers[0].type = &cenum_def;
	  umembers[0].name = get_symbol("cenum");
	}
	
	{//simple
	  static type_def simple_def;
	  static decl members[2];
	  simple_def.type = STRUCT;
	  simple_def.cstruct.name=get_symbol("_simple");
	  simple_def.cstruct.members = members;
	  simple_def.cstruct.cnt = array_count(members);
	  members[0].name = get_symbol("name");
	  members[0].type = &symbol_def;
	  members[1].name = get_symbol("cname");
	  members[1].type = &symbol_def;
	  umembers[1].type = &simple_def;
	  umembers[1].name = get_symbol("simple");
	}
	
	{//fcn
	  static type_def fcn_def;
	  static decl members[3];
	  fcn_def.type = STRUCT;
	  fcn_def.cstruct.cnt = array_count(members);
	  fcn_def.cstruct.members = members;
	  fcn_def.cstruct.name = get_symbol("_fcn");
	  members[0].name= get_symbol("ret");
	  members[0].type = &type_def_ptr_def;
	  members[1].name = get_symbol("args");
	  members[1].type = &type_def_ptr_def;
	  members[2].name = get_symbol("cnt");
	  members[2].type = &i64_def;
	  umembers[2].type = &fcn_def;
	  umembers[2].name = get_symbol("fcn");
	}
	
	{//cstruct/cunion
	  static decl cstruct_members[3];
	  cstruct_members[0].name = get_symbol("name");
	  cstruct_members[0].type = &symbol_def;
	  cstruct_members[1].name = get_symbol("members");
	  cstruct_members[1].type = &decl_ptr_def;
	  cstruct_members[2].name = get_symbol("cnt");
	  cstruct_members[2].type = &i64_def;
	  static type_def cstruct_def;
	  static type_def cunion_def;
	  cstruct_def.type = STRUCT;
	  cstruct_def.cstruct.name = get_symbol("_cstruct");
	  cstruct_def.cstruct.members= cstruct_members;
	  cstruct_def.cstruct.cnt = array_count(cstruct_members);
	  cunion_def = cstruct_def;
	  cunion_def.cstruct.name = get_symbol("_cunion");

	  umembers[3].type = &cstruct_def;
	  umembers[3].name = get_symbol("cstruct");
	  umembers[4].type = &cunion_def;
	  umembers[4].name = get_symbol("cunion");
	}
	
	{//ptr
	  static decl members[1];
	  members[0].name = get_symbol("inner");
	  members[0].type = &type_def_ptr_def;
	  static type_def ptr_def;
	  ptr_def.type = STRUCT;
	  ptr_def.cstruct.members = members;
	  ptr_def.cstruct.cnt = 1;
	  ptr_def.cstruct.name = get_symbol("_ptr");
	  umembers[5].type = &ptr_def;
	  umembers[5].name = get_symbol("ptr");
	}
	{// typedef
	  static decl members[2];
	  members[0].name = get_symbol("name");
	  members[0].type = &symbol_def;
	  members[1].name = get_symbol("inner");
	  members[1].type = &type_def_ptr_def;
	  static type_def ctypedef_def;
	  ctypedef_def.type = STRUCT;
	  ctypedef_def.cstruct.name = get_symbol("_ctypedef");
	  ctypedef_def.cstruct.members = members;
	  ctypedef_def.cstruct.cnt = array_count(members);
	  umembers[6].type = &ctypedef_def;
	  umembers[6].name = get_symbol("ctypedef");
	}	
      }
    }
  }
  {
    static decl members[2];
    static type_def dclinner;
    members[0].name = get_symbol("name");
    members[0].type = &char_ptr_def;
    members[1].name = get_symbol("type");
    members[1].type = &type_def_def;
     
    dclinner.type = STRUCT;
    dclinner.cstruct.name = get_symbol("_decl");
    dclinner.cstruct.members = members;
    dclinner.cstruct.cnt = array_count(members);
    
    decl_def.type = TYPEDEF;
    decl_def.ctypedef.name = get_symbol("decl");
    decl_def.ctypedef.inner = &dclinner;
  }

  r2(&type_def_def);
  r2(&decl_def);
  r2(&type_def_ptr_def);
  r2(&decl_ptr_def);

  { // fcn_def
    fcn_def_def.type = TYPEDEF;
    static decl members[4];
    static type_def inner;
    fcn_def_def.ctypedef.name = get_symbol("fcn_def");
    fcn_def_def.ctypedef.inner = &inner;
    inner.type = STRUCT;
    inner.cstruct.members = members;
    inner.cstruct.cnt = array_count(members);
    inner.cstruct.name = get_symbol("_fcn_def");
  
    members[0].name = get_symbol("name");
    members[0].type = &char_ptr_def;
    members[1].name = get_symbol("type");
    members[1].type = &type_def_def;
    members[2].name = get_symbol("is_extern");
    members[2].type = &u8_def;
    members[3].name = get_symbol("ptr");
    members[3].type = &void_ptr_def;
    r2(&fcn_def_def);
  }
  
  { // cmacrodef_def
    cmacro_def_def.type = TYPEDEF;
    static decl members[3];
    static type_def inner;
    cmacro_def_def.ctypedef.name = get_symbol("cmacro_def");
    cmacro_def_def.ctypedef.inner = &inner;
    inner.type = STRUCT;
    inner.cstruct.members = members;
    inner.cstruct.cnt = array_count(members);
    inner.cstruct.name = get_symbol("_cmacro_def");
    members[0].name = get_symbol("name");
    members[0].type = &char_ptr_def;
    members[1].name = get_symbol("arg_cnt");
    members[1].type = &i64_def;
    members[2].name = get_symbol("fcn");
    members[2].type = &void_ptr_def;
    r2(&cmacro_def_def);
  }

}

