//required lisp_types.h

type_def * type_pool_get(type_def * lookup);
void type_pool_reg_static(type_def * static_type);
type_def * type_pool_simple(symbol s);
bool test_type_pool();
