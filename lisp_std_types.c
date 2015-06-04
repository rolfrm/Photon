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

static void r(type_def * def){
    register_type(def, def->simple.name);
}

void r2(type_def * def){
  register_type(def,NULL);
}

// Loads a all the types we need.
// call before anything else.
void load_defs(){
  // simple types //
  void_def = make_simple("void", "void");
  r(&void_def);
  void_ptr_def = make_ptr(&void_def);
  r2(&void_ptr_def);
  error_def = make_simple("error","error");
  r2(&error_def);
  char_def = make_simple("char", "char");
  r2(&char_def);
  i64_def = make_simple("i64", "i64");
  r2(&i64_def);
  i32_def = make_simple("i32", "i32");
  r2(&i32_def);
  u8_def = make_simple("u8", "u8");
  r2(&u8_def);
  f32_def = make_simple("f32", "f32");
  r2(&f32_def);
  f64_def = make_simple("f64", "f64");
  r2(&f64_def);
  // pointers to simple types //
  char_ptr_def.kind = POINTER;
  char_ptr_def.ptr.inner = &char_def;
  r2(&char_ptr_def);
  char_ptr_ptr_def.kind = POINTER;
  char_ptr_ptr_def.ptr.inner = &char_ptr_def;
  r2(&char_ptr_ptr_def);
  i64_ptr_def.kind = POINTER;
  i64_ptr_def.ptr.inner = &i64_def;  
  r2(&i64_ptr_def);
  type_def_def.kind = TYPEDEF;
  type_def_def.ctypedef.name = "type_def";
  type_def_ptr_def.kind = POINTER;
  type_def_ptr_def.ptr.inner = &type_def_def;  


  decl_ptr_def.kind = POINTER;
  decl_ptr_def.ptr.inner = &decl_def;
  
  { // kind enum
    static type_def type_def_kind_def_inner;
    type_def_kind_def.kind = TYPEDEF;
    type_def_kind_def.ctypedef.inner = &type_def_kind_def_inner;
    type_def_kind_def.ctypedef.name = "type_def_kind";
    static char * kindnames[] = {"SIMPLE", "FUNCTION", "POINTER", "STRUCT", "UNION", "ENUM"};
    static i64 kindvalues[] = {SIMPLE, FUNCTION, POINTER, STRUCT, UNION, ENUM};
    type_def_kind_def_inner.kind = ENUM;
    type_def_kind_def_inner.cenum.cnt = array_count(kindnames);
    type_def_kind_def_inner.cenum.names = kindnames;
    type_def_kind_def_inner.cenum.values = kindvalues;
    r2(&type_def_kind_def);
  }
    
  { //type_def struct members:
    static type_def itype_def_def;
    type_def_def.ctypedef.inner = &itype_def_def;
    
    static decl members[2];
    itype_def_def.kind = STRUCT;
    itype_def_def.cstruct.members = members;
    itype_def_def.cstruct.cnt = array_count(members);
    itype_def_def.cstruct.name = "_type_def";
    
    members[0].type = &type_def_kind_def;
    members[0].name = "kind";
    {
      static type_def type_def_union;
      static decl umembers[7];
      type_def_union.kind = UNION;    
      type_def_union.cunion.name = NULL;
      type_def_union.cunion.cnt = array_count(umembers);
      type_def_union.cunion.members = umembers;
      //      r2(&type_def_union);
      
      members[1].type = &type_def_union;

      members[1].name = NULL;

      {// anon union members
	
	{//cenum
	  static type_def cenum_def;
	  static decl cenum_members[4];
	  cenum_members[0].name = "names";
	  cenum_members[0].type = &char_ptr_ptr_def;
	  cenum_members[1].name = "values";
	  cenum_members[1].type = &i64_ptr_def;
	  cenum_members[2].name = "cnt";
	  cenum_members[2].type = &i64_def;
	  cenum_members[3].name = "enum_name";
	  cenum_members[3].type = &char_ptr_def;
	  
	  cenum_def.kind = STRUCT;
	  cenum_def.cstruct.members = cenum_members;
	  cenum_def.cstruct.name = "_enum";
	  cenum_def.cstruct.cnt = array_count(cenum_members);
	  r2(&cenum_def);
	  umembers[0].type = &cenum_def;
	  umembers[0].name = "cenum";
	}
	
	{//simple
	  static type_def simple_def;
	  static decl members[2];
	  simple_def.kind = STRUCT;
	  simple_def.cstruct.name="_simple";
	  simple_def.cstruct.members = members;
	  simple_def.cstruct.cnt = array_count(members);
	  members[0].name ="name";
	  members[0].type = &char_ptr_def;
	  members[1].name = "cname";
	  members[1].type = &char_ptr_def;
	  umembers[1].type = &simple_def;
	  umembers[1].name = "simple";
	  r2(&simple_def);
	}
	
	{//fcn
	  static type_def fcn_def;
	  static decl members[3];
	  fcn_def.kind = STRUCT;
	  fcn_def.cstruct.cnt = array_count(members);
	  fcn_def.cstruct.members = members;
	  fcn_def.cstruct.name = "_fcn";
	  members[0].name= "ret";
	  members[0].type = &type_def_ptr_def;
	  members[1].name = "args";
	  members[1].type = &type_def_ptr_def;
	  members[2].name = "cnt";
	  members[2].type = &i64_def;
	  umembers[2].type = &fcn_def;
	  umembers[2].name = "fcn";
	  r2(&fcn_def);
	}
	
	{//cstruct/cunion
	  static decl cstruct_members[3];
	  cstruct_members[0].name = "name";
	  cstruct_members[0].type = &char_ptr_def;
	  cstruct_members[1].name = "members";
	  cstruct_members[1].type = &decl_ptr_def;
	  cstruct_members[2].name = "cnt";
	  cstruct_members[2].type = &i64_def;
	  static type_def cstruct_def;
	  static type_def cunion_def;
	  cstruct_def.kind = STRUCT;
	  cstruct_def.cstruct.name = "_cstruct";
	  cstruct_def.cstruct.members= cstruct_members;
	  cstruct_def.cstruct.cnt = array_count(cstruct_members);
	  cunion_def = cstruct_def;
	  cunion_def.cstruct.name = "_cunion";

	  umembers[3].type = &cstruct_def;
	  umembers[3].name = "cstruct";
	  umembers[4].type = &cunion_def;
	  umembers[4].name = "cunion";
	  r2(&cstruct_def);
	  r2(&cunion_def);
	}
	
	{//ptr
	  static decl members[1];
	  members[0].name = "inner";
	  members[0].type = &type_def_ptr_def;
	  static type_def ptr_def;
	  ptr_def.kind = STRUCT;
	  ptr_def.cstruct.members = members;
	  ptr_def.cstruct.cnt = 1;
	  ptr_def.cstruct.name = "_ptr";
	  umembers[5].type = &ptr_def;
	  umembers[5].name = "ptr";
	  r2(&ptr_def);
	}
	{// typedef
	  static decl members[2];
	  members[0].name = "name";
	  members[0].type = &char_ptr_def;
	  members[1].name = "inner";
	  members[1].type = &type_def_ptr_def;
	  static type_def ctypedef_def;
	  ctypedef_def.kind = STRUCT;
	  ctypedef_def.cstruct.name = "_ctypedef";
	  ctypedef_def.cstruct.members = members;
	  ctypedef_def.cstruct.cnt = array_count(members);
	  r2(&ctypedef_def);
	  umembers[6].type = &ctypedef_def;
	  umembers[6].name = "ctypedef";
	}
	
      }
    }
  }
  {
    static decl members[2];
    static type_def dclinner;
    members[0].name = "name";
    members[0].type = &char_ptr_def;
    members[1].name = "type";
    members[1].type = &type_def_def;
     
    dclinner.kind = STRUCT;
    dclinner.cstruct.name = "_decl";
    dclinner.cstruct.members = members;
    dclinner.cstruct.cnt = array_count(members);
    
    decl_def.kind = TYPEDEF;
    decl_def.ctypedef.name = "decl";
    decl_def.ctypedef.inner = &dclinner;
  }
  r2(&type_def_def);
  r2(&decl_def);
  r2(&type_def_ptr_def);
  r2(&decl_ptr_def);

  { // fcn_def
    fcn_def_def.kind = TYPEDEF;
    static decl members[4];
    static type_def inner;
    fcn_def_def.ctypedef.name = "fcn_def";
    fcn_def_def.ctypedef.inner = &inner;
    inner.kind = STRUCT;
    inner.cstruct.members = members;
    inner.cstruct.cnt = array_count(members);
    inner.cstruct.name = "_fcn_def";
  
    members[0].name = "name";
    members[0].type = &char_ptr_def;
    members[1].name = "type";
    members[1].type = &type_def_def;
    members[2].name = "is_extern";
    members[2].type = &u8_def;
    members[3].name = "ptr";
    members[3].type = &void_ptr_def;
    r2(&fcn_def_def);
  }
  
  { // cmacrodef_def
    cmacro_def_def.kind = TYPEDEF;
    static decl members[3];
    static type_def inner;
    cmacro_def_def.ctypedef.name = "cmacro_def";
    cmacro_def_def.ctypedef.inner = &inner;
    inner.kind = STRUCT;
    inner.cstruct.members = members;
    inner.cstruct.cnt = array_count(members);
    inner.cstruct.name = "_cmacro_def";
    members[0].name = "name";
    members[0].type = &char_ptr_def;
    members[1].name = "arg_cnt";
    members[1].type = &i64_def;
    members[2].name = "fcn";
    members[2].type = &void_ptr_def;
    r2(&cmacro_def_def);
  }
}

