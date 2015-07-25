#include <stdio.h>
#include <iron/full.h>
#include "lisp_parser.h"
#include <libtcc.h>
#include "lisp_types.h"
#include "c_ast.h"
#include "lisp_compiler.h"
#include "lisp_std_types.h"
#include <ctype.h>

#include "uthash.h"

typedef struct{
  char * key;
  u64 value;
  UT_hash_handle hh;
  UT_hash_handle hh2;
}symbol_table;
const symbol symbol_empty = {0};
symbol_table * symtbl = NULL; 
symbol_table * symtbl_byid = NULL; 
u32 symbol_cnt = 1;

symbol * get_symbol2(char * name){
  if(name == NULL)
    return (symbol *) &symbol_empty;
  
  symbol_table * sym_item = NULL;
  HASH_FIND_STR(symtbl, name, sym_item);
  if(sym_item == NULL){
    
    sym_item = alloc0(sizeof(symbol_table));
    sym_item->value = ++symbol_cnt;
    sym_item->key = fmtstr("%s", name);
    HASH_ADD_KEYPTR(hh, symtbl, sym_item->key, strlen(sym_item->key), sym_item);
    HASH_ADD(hh2, symtbl_byid, value, sizeof(sym_item->value), sym_item);
  }
  return (symbol *) &sym_item->value;
}

symbol get_symbol(char * name){
  return *get_symbol2(name);
}

char * symbol_name(symbol s){
  symbol_table * sym_item = NULL;
  HASH_FIND(hh2, symtbl_byid, &s.id,sizeof(s.id), sym_item);
  if(sym_item == NULL)
    return NULL;
  return sym_item->key;
}
bool symbol_cmp(symbol a, symbol b){
  return (a.id == b.id);
}

typedef struct _symbol_stack symbol_stack;
struct _symbol_stack{
  var_def ** vars;
  size_t *vars_cnt;
};

static __thread symbol_stack symbolstacks[100];
static size_t stack_count = 0;

void push_symbols(var_def ** vars, size_t * vars_cnt){
  symbolstacks[stack_count].vars = vars;
  symbolstacks[stack_count].vars_cnt = vars_cnt;  
  stack_count++;
}

void pop_symbols(){
  stack_count--;
}

var_def * get_stack_variable(symbol name){

  for(i64 j = stack_count-1; j >= 0; j--){
    symbol_stack ss = symbolstacks[j];
    var_def * vars = *ss.vars;
    size_t varcnt = *ss.vars_cnt;
    for(size_t i = 0;i < varcnt; i++){
      if(!symbol_cmp(name,vars[i].name)){
	goto next_item;
      }
      return vars + i;
    next_item:
      continue;
    }
  }
  return NULL;
}

#include "uthash.h"

typedef struct {
  char * key;
  char * cname;
  UT_hash_handle hh;
}symbol_lookup;

// Converts from base radix A numbers to radix B.
void basea2b(bool (* read_glyph)(u8 * ctn), u32 from_base, u32 to_base, void (*emit_glyph)(u8 glyph)){
  
  double bitstack = 0;
  double basecount = 1;

  while(true){
    while(basecount < to_base){
      u8 chr;
      if(!read_glyph(&chr)) goto exit;
      bitstack = bitstack + basecount * chr;
      basecount *=  from_base;
    }

    emit_glyph((u32)bitstack % to_base);
    bitstack /= to_base;
    basecount /= to_base;
  }
 exit:
  if(basecount > 1)
    emit_glyph(bitstack);
}


char base61char(int i){
  if(i < 10)
    return '0' + i;
  if(i < 10 + 26)
    return 'A' + (i - 10);
  if(i < 10 + 26 + 26)
    return 'a' + (i - 10 - 26);
  if(i == 62) return '_';
  return '=';
}

int ibase61char(char c){
  if(c >= '0' && c <= '9')
    return c - '0';
  if(c >= 'A' && c<= 'Z')
    return c - 'A' + 10;
  if(c >= 'a' && c <= 'z')
    return c - 'a' + 10 + 26;
  if(c == '_')
    return 62; //'_'
  else return 63;
}

void base61format(char * str, char * output){
  bool next_glyph(u8 * chr){
    if(*str == 0) return false;
    *chr = *str;
    str++;
    return true;
  }
  void emit_glyph(u8 chr){
    *output = base61char(chr);
    output++;
    *output = 0;
  }
  basea2b(next_glyph, 256, 63, emit_glyph);
}

char cname[1000];
char * get_c_name(symbol s){
  char * sym = symbol_name(s);
  bool first_alpha = isalpha(sym[0]) || '_' == sym[0];
  bool all_alphanum = true;
  for(size_t i = 0; sym[i];i++){
    if(isalnum(sym[i]) == false && sym[i] !='_'){
      all_alphanum = false;
      break;
    }
  }
  
  if(all_alphanum){
    if(first_alpha) return sym;
    sprintf(cname, "S_%s", sym);
    return cname;
  }else{
    sprintf(cname, "S__");
    base61format(sym, cname + 3);
    return cname;
  }
}

void format_c_name(symbol s){
  format("%s", get_c_name(s));
}

bool test_get_cname(){
  char * get_c(char * str){return get_c_name(get_symbol(str));};
  char * n1 = get_c("thingy");
  char * n2 = get_c("2thingy");
  char * n3 = get_c("321");
  char * n4 = get_c("+");
  char * n5 = get_c("things-and-epic-things");
  char * n6 = get_c("+");
  char * n7 = get_c("+plus");
  char * n8 = get_c("+plus");
  
  logd("%s %s %s %s %s %s %s %s\n",n1, n2, n3, n4, n5, n6, n7, n8);
  TEST_ASSERT(n6 == n4);
  TEST_ASSERT(n7 == n8);
  return TEST_SUCCESS;
}

bool test_symbol_table(){
  symbol s = get_symbol("test");
  symbol s2 = get_symbol("__TEST__");
  symbol s3 = get_symbol("test");
  symbol s4 = get_symbol("__TEST__");
  TEST_ASSERT(symbol_cmp(s2, s4) && symbol_cmp(s, s3) && !symbol_cmp(s, s2));
  TEST_ASSERT(strcmp(symbol_name(s3), "test") == 0);
  TEST_ASSERT(strcmp(symbol_name(s4), "__TEST__") == 0);
  int scnt = 1000;
  symbol * symbols = alloc(sizeof(symbol) * scnt);
  for(int i = 0; i < scnt; i++){
    char buf[20];
    sprintf(buf, "s%i", i);
    symbols[i] = get_symbol(buf);
  }					       
  u64 lastid = -1;
  for(int i = 0; i < scnt; i++){
    char buf[20];
    sprintf(buf, "s%i", i);
    TEST_ASSERT(get_symbol(buf).id == symbols[i].id);
    TEST_ASSERT(get_symbol(buf).id != lastid);
    lastid = get_symbol(buf).id;
  }
  
  for(int i = 0 ; i < 63; i++){
    char mangled = base61char(i);
    int demangled = ibase61char(mangled);
    format("%i: %c %i\n", i, mangled, demangled);
    TEST_ASSERT(i == demangled);
  }

  char testbuf[100];
  char testinput[] = "e210hye291";
  base61format(testinput, testbuf);
  logd("%s\n",testbuf);

  return TEST_SUCCESS;
}

bool test_symbols(){
  TEST(test_get_cname);
  TEST(test_symbol_table);
  return TEST_SUCCESS;
}


