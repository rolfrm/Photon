// requires c types
typedef struct _c_fcndef c_fcndef;
typedef struct _c_expr c_expr;

typedef struct{
  c_expr * exprs;
  size_t expr_cnt;
}c_block;

struct _c_fcndef{
  symbol name;
  type_def * type;
  symbol * args;
  c_block block;
};

typedef enum{
  C_VAR = 2,
  C_VALUE = 4,
  C_VALUE_UNENDED = 5,
  C_RETURN = 6,
  C_BLOCK = 7,
  C_KEYWORD = 8
}c_expr_kind;

typedef enum{
  C_SUB_EXPR = 11,
  C_INLINE_VALUE = 12,
  C_FUNCTION_CALL = 13,
  C_OPERATOR,
  C_DEREF,
  C_ADDRESS_OF,
  C_SYMBOL,
  C_CAST,
  C_NOTHING,
  C_MEMBER
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
  char * operator;
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

typedef struct{
  symbol name;
  c_value * item;
  type_def * type;
}c_member;

typedef struct{
  c_value * inner;
  type_def * return_type;
}c_deref;

struct _c_value{
  c_value_kind type;
  union{
    c_cast cast;
    c_raw_value raw; //inline
    symbol symbol;
    c_value * value;//sub expr, address of
    c_function_call call;
    c_operator operator;
    c_member member;
    c_deref deref;
  };  
};

struct _c_expr{
  c_expr_kind type;
  union{
    c_var var;
    // return, value
    c_value value;
    c_block block;
    symbol keyword;

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

// Calculates the type and variable dependencies for a c_root_code.
void c_root_code_dep(type_def ** deps, symbol* vdeps, c_root_code code);

// Calculates the type and variable dependencies for a c code block.
void block_dep(type_def ** deps, symbol * vdeps, c_block blk);

// Adds an expression to a block.
void block_add(c_block * blk, c_expr expr);

// ** Cleanup ** //

// Cleans up the memory used by the code.
void c_root_code_delete(c_root_code code);
void c_block_delete(c_block blk);
void c_value_delete(c_value val);
void c_expr_delete(c_expr expr);
extern const c_block c_block_empty;
extern const c_value c_value_empty;
extern const c_expr c_expr_block;
extern const c_expr c_expr_value;
c_value c_value_sub_expr(c_value * val);
// format out the c-code
void print_c_code(c_root_code code);

// test
bool test_print_c_code();
