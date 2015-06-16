// Requires bitguy.h
typedef enum {
  SIMPLE = 0,
  FUNCTION = 1,
  POINTER = 2,
  STRUCT = 3,
  UNION = 4,
  ENUM = 5,
  TYPEDEF = 6,
  OPAQUE_STRUCT,
  type_def_kind_cnt
} type_def_kind;

typedef struct{
  u64 id;
}symbol;

struct _type_def;
typedef struct _type_def type_def;
struct _decl;
typedef struct _decl decl;

struct{
  type_def * types;
  symbol * names;
  i64 cnt;
}members;

struct _type_def{
  type_def_kind type;
  union{
    struct _enum{
      symbol * names;
      i64 * values;
      i64 cnt;
      symbol name;
    }cenum;

    struct _simple{
      symbol name;
      size_t size;
    }simple;
    
    struct{
      type_def * ret;
      type_def ** args;
      i64 cnt;
    }fcn;
    
    struct{
      symbol name; // can be empty
      decl * members;
      i64 cnt;
    }cstruct;

    struct{
      symbol name; // can be empty
      decl * members;
      i64 cnt;
      i8 opaque;
    }cunion;

    struct{
      type_def * inner;
    }ptr;

    struct{
      symbol name;
      type_def * inner;
    }ctypedef;
  };
};

struct _decl{
  symbol name;
  type_def * type;
};

#include "c_ast.h"

size_t get_sub_type_cnt(type_def * t);
void get_sub_types(type_def * t, type_def ** out_types);

extern const symbol symbol_empty;
symbol  get_symbol(char * name);
symbol * get_symbol2(char * name);
char * symbol_name(symbol s);
bool symbol_cmp(symbol a, symbol b);
u64 size_of(type_def * t);
type_def * function_type(type_def * ret,size_t cnt, type_def ** ts);

void print_cdecl(decl idecl);

type_def make_simple(char * name, size_t s);
type_def make_ptr(type_def * def);
/*type_def * get_type_def(type_def def);
type_def * get_type_from_string(char * name);
void register_type(type_def * type, char * name);
*/
// simple function to calculate type dependencies.
// writes the dependencies of a type in defs
// descending order, so least dependent comes first.
void make_dependency_graph(type_def ** deps, type_def * def);

void print_def(type_def * type);
void print_min_type(type_def * type);
type_def * str2type(char * str);
void print_decl(type_def * t, symbol name);
