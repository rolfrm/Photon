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
#include "lisp_types.h"
#include "lisp_parser.h"
#include "lisp_compiler.h"
#include "lisp_std_types.h"
#include "type_pool.h"
static size_t known_types_cnt[type_def_kind_cnt] = {0};
static type_def ** known_types[type_def_kind_cnt] = {NULL};

static bool compare_simple(type_def * a, type_def * b){
  return a->simple.name.id == b->simple.name.id;
}

static bool compare_pointer(type_def * a, type_def * b){
  return type_pool_get(a->ptr.inner) == type_pool_get(b->ptr.inner);
}

static bool compare_struct(type_def * a, type_def * b){
  return a->cstruct.name.id == b->cstruct.name.id;
}

static bool compare_enum(type_def * a, type_def * b){
  return a->cenum.name.id == b->cenum.name.id;
}

static bool compare_typedef(type_def * a, type_def * b){
  return a->ctypedef.name.id ==  b->ctypedef.name.id;
}
	  
static bool compare_function(type_def * a, type_def * b){
  bool isSame =  a->fcn.cnt == b->fcn.cnt && type_pool_get(a->fcn.ret) == type_pool_get(b->fcn.ret);
  if(isSame){
    for(i64 i = 0; i < a->fcn.cnt; i++){
      if(type_pool_get(a->fcn.args[i].type) != type_pool_get(b->fcn.args[i].type))
	return false;
    }
  }
  return isSame;
}
	  
type_def * _type_pool_get(type_def * lookup, bool is_static){
  type_def *** kind_array = known_types + lookup->type;
  size_t * cnt = known_types_cnt + lookup->type;
  bool (*cmp)(type_def *, type_def *) = NULL;
  switch(lookup->type){
  case SIMPLE:
    cmp = compare_simple;
    break;
  case POINTER:
    cmp = compare_pointer;
    break;
  case UNION:
  case STRUCT:
    cmp = compare_struct;
    break;
  case ENUM:
    cmp = compare_enum;
    break;
  case TYPEDEF:
    cmp = compare_typedef;
    break;
  case FUNCTION:
    cmp = compare_function;
    break;
  default:
    ERROR("Unsupported type");
    break;
  }

  type_def * found = NULL;
  for(size_t i = 0; i < *cnt; i++)
    if(lookup == (*kind_array)[i])
      return lookup;
  
  for(size_t i = 0; i < *cnt; i++){
    if(cmp(lookup, (*kind_array)[i])){
      found = (*kind_array)[i];
      break;
    }
  }
  if(is_static && found)
    return found;
  static size_t anon_type_id = 0;
  char * anonsym;
  if(found == NULL){
    
    found = is_static ? lookup : clone(lookup, sizeof(*lookup));
    list_add((void **) kind_array, cnt, &found, sizeof(type_def *)); 
    
    type_def * (* do_lookup)(type_def *) = lambda(type_def *, (type_def * a){return _type_pool_get(a, is_static);});

    switch(lookup->type){
    case SIMPLE:
    case ENUM:
      break;
    case POINTER:
      found->ptr.inner = do_lookup(found->ptr.inner);
      break;
    case TYPEDEF:
      found->ctypedef.inner = do_lookup(found->ctypedef.inner);
      break;
    case UNION:
    case STRUCT:
      if(found->cstruct.name.id == symbol_empty.id){
	anonsym = fmtstr("__anon%i",anon_type_id++);
	found->cstruct.name = get_symbol(anonsym);
	dealloc(anonsym);
      }
      for(int i = 0; i < found->cstruct.cnt; i++){
	found->cstruct.members[i].type = do_lookup(found->cstruct.members[i].type);
      }
      break;
    case FUNCTION:
      found->fcn.ret = do_lookup(found->fcn.ret);
      for(int i = 0 ; i < found->fcn.cnt; i++){
	found->fcn.args[i].type = do_lookup(found->fcn.args[i].type);
      }
      break;
    default:
      ERROR("Unsupported type");
      break;
    }
  }
  ASSERT(found != NULL);
  return found;
}

type_def * type_pool_get(type_def * lookup){
  return _type_pool_get(lookup, false);
}

void type_pool_reg_static(type_def * lookup){
  _type_pool_get(lookup, true);
}


type_def * type_pool_simple(symbol s){
  type_def_kind valid_kinds[] = {SIMPLE, TYPEDEF, STRUCT, UNION, ENUM};
  for(size_t j = 0; j < array_count(valid_kinds); j++){
    size_t c = known_types_cnt[valid_kinds[j]];
    type_def ** td = known_types[valid_kinds[j]];
    switch(valid_kinds[j]){
    case SIMPLE:
      for(size_t i = 0; i < c; i++)
	if(td[i]->simple.name.id == s.id) return td[i];
      break;
    case TYPEDEF:
      for(size_t i = 0; i < c; i++)
	if(td[i]->ctypedef.name.id == s.id) return td[i];
      break;
    case STRUCT:
    case UNION:
      for(size_t i = 0; i < c; i++)
	if(td[i]->cstruct.name.id == s.id) return td[i];
      break;
    case ENUM:
      for(size_t i = 0; i < c; i++)
	if(td[i]->cenum.name.id == s.id) return td[i];
      break;
    default:
      ERROR("Unsupported type");
      break;
    }
  }	  
  
  return NULL;
}

	  
bool test_type_pool(){
  //type_def * td = str2type("(ptr i64)");
  type_def d;
  d.type = SIMPLE;
  d.simple.name = get_symbol("i64");
  d.simple.size = sizeof(i64);
  type_def * td = type_pool_get(&d);
  TEST_ASSERT(td != NULL);
  TEST_ASSERT(td == type_pool_get(&d));
  type_def d2;
  d2.type = POINTER;
  d2.ptr.inner = td;
  TEST_ASSERT(type_pool_get(&d2) == type_pool_get(&d2));

  return TEST_SUCCESS;
}
