// requires lisp_types.h

// this file includes all the basic type definitions for lisp.
// call load_defs before using any lisp.

// Simple
type_def void_def;
type_def char_def;
type_def i64_def;
type_def i32_def;
type_def i16_def;
type_def i8_def;
type_def u64_def;
type_def u32_def;
type_def u16_def;
type_def u8_def;
type_def f32_def;
type_def f64_def;

// Pointers
type_def void_ptr_def;
type_def char_ptr_def;
type_def char_ptr_ptr_def;
type_def i64_ptr_def;

// Aggregate types
type_def type_def_def;
type_def type_def_kind_def;
type_def type_def_ptr_def;
type_def decl_def;
type_def decl_ptr_def;
type_def error_def;
type_def fcn_def_def;
type_def cmacro_def_def;
type_def symbol_def;

void load_defs();
