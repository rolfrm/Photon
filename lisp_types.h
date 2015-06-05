// Requires bitguy.h
typedef enum {
  SIMPLE = 0,
  FUNCTION = 1,
  POINTER = 2,
  STRUCT = 3,
  UNION = 4,
  ENUM = 5,
  TYPEDEF = 6,
  type_def_kind_cnt
} type_def_kind;

typedef struct{
  u64 id;
}symbol;

struct _type_def;
typedef struct _type_def type_def;
struct _decl;
typedef struct _decl decl;

struct _type_def{
  type_def_kind type;
  union{
    struct _enum{
      symbol * names;
      i64 * values;
      i64 cnt;
      symbol enum_name;
    }cenum;

    struct _simple{
      symbol name;
    }simple;
    
    struct{
      type_def * ret;
      // todo: consider redesigning so thar fcn args are just type_defs instead.
      // the function type has really nothing to do with the arg names and hence should not be declerations.
      // but: function types are complicated and backwards to write in c, so this is a tradeoff to make it easy.
      decl * args;
      i64 cnt;
    }fcn;

    struct{
      symbol name; // NULL if anonymous
      decl * members;
      i64 cnt;
    }cstruct;

    struct{
      symbol name; // NULL if anonymous
      decl * members;
      i64 cnt;
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

typedef struct _c_fcndef c_fcndef;
typedef struct _c_expr c_expr;

typedef struct{
  c_expr * exprs;
  size_t expr_cnt;
}c_block;

struct _c_fcndef{
  decl fdecl;
  c_block block;
};

typedef enum{
  C_VAR = 2,
  C_VALUE = 4,
  C_RETURN = 6,
  C_BLOCK = 7
}c_expr_kind;

typedef enum{
  C_SUB_EXPR,
  C_INLINE_VALUE,
  C_FUNCTION_CALL,
  C_OPERATOR,
  C_DEREF,
  C_SYMBOL,
  C_CAST
}c_value_kind;

struct _c_value;
typedef struct _c_value c_value;

typedef struct{
  decl var;
  c_value * value; //if NULL no value
}c_var;

typedef struct{
  type_def * type;
  c_value * value;
}c_cast;

typedef struct{
  c_value * left;
  c_value * right;
  char operator;
}c_operator;

typedef struct{
  char * value;
  type_def * type;
}c_raw_value;

typedef struct{
  symbol name;
  c_value * args;
  size_t arg_cnt;
  type_def * type;
}c_function_call;

struct _c_value{
  c_value_kind type;
  union{
    c_cast cast;
    c_raw_value raw;
    symbol symbol;
    c_value * value;//sub expr, deref
    c_function_call call;
    c_operator operator;
  };  
};

struct _c_expr{
  c_expr_kind type;
  union{
    c_var var;
    // return, value
    c_value value;
    c_block block;
  };
};

typedef enum{
  C_FUNCTION_DEF,
  C_VAR_DEF,
  C_INCLUDE,
  C_INCLUDE_LIB,
  C_DECL,
  C_TYPE_DEF
}c_root_code_kind;

typedef struct{
  c_root_code_kind type;
  union{
    char * include;
    c_fcndef fcndef;
    c_var var;
    decl decl;
    type_def * type_def;
  };
}c_root_code;

extern const symbol symbol_empty;
symbol  get_symbol(char * name);
symbol * get_symbol2(char * name);
char * symbol_name(symbol s);
bool symbol_cmp(symbol a, symbol b);

void print_cdecl(decl idecl);

type_def make_simple(char * name);
type_def make_ptr(type_def * def);
type_def * get_type_def(type_def def);
type_def * get_type_from_string(char * name);
void register_type(type_def * type, char * name);

// simple function to calculate type dependencies.
// writes the dependencies of a type in defs
// descending order, so least dependent comes first.
void make_dependency_graph(type_def ** deps, type_def * def);


// Calculates the type and variable dependencies for a c_root_code.
void c_root_code_dep(type_def ** deps, symbol* vdeps, c_root_code code);
// Calculates the type and variable dependencies for a c code block.
void block_dep(type_def ** deps, symbol * vdeps, c_block blk);


void get_var_dependencies(symbol * type_names, c_root_code * code);
void print_def(type_def * type, bool is_decl);

void print_c_code(c_root_code code);

// test
bool test_print_c_code();
