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

static size_t known_types_cnt[type_def_kind_cnt] = {0};
static type_def ** known_types[type_def_kind_cnt] = {NULL};

static bool compare_simple(type_def * a, type_def * b){
  return a->simple.name.id == b->simple.name.id;
}

static bool compare_pointer(type_def * a, type_def * b){
  return a->ptr.inner == b->ptr.inner;
}

static bool compare_struct(type_def * a, type_def * b){
   return a->cstruct.name.id == b->cstruct.name.id;
}

type_def * type_pool_get(type_def lookup){
  type_def *** kind_array = known_types + lookup.type;
  size_t * cnt = known_types_cnt + lookup.type;
  bool (*cmp)(type_def *, type_def *) = NULL;
  logd("found: %p %p %i\n", *kind_array, cnt, *cnt);
  switch(lookup.type){
  case SIMPLE:
    cmp = compare_simple;
    break;
  case POINTER:
    cmp = compare_pointer;
    break;
  case STRUCT:
    cmp = compare_struct;
    break;
  default:
    ERROR("Unsupported type");
    break;
  }

  type_def * found = NULL;
  for(size_t i = 0; i < *cnt; i++){
    if(cmp(&lookup, (*kind_array)[i])){
      found = (*kind_array)[i];
      break;
    }
  }
  
  if(found == NULL){
    found = clone(&lookup, sizeof(lookup));
    list_add((void **) kind_array, cnt, &found, sizeof(type_def *)); 
  }
  return found;
}

bool test_type_pool(){
  //type_def * td = str2type("(ptr i64)");
  type_def d;
  d.type = SIMPLE;
  d.simple.name = get_symbol("i64");
  type_def * td = type_pool_get(d);
  TEST_ASSERT(td != NULL);
  TEST_ASSERT(td == type_pool_get(d));
  type_def d2;
  d2.type = POINTER;
  d2.ptr.inner = td;
  TEST_ASSERT(type_pool_get(d2) == type_pool_get(d2));

  return TEST_SUCCESS;
}
