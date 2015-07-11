#include <stdio.h>
#include <iron/full.h>
#include "lisp_parser.h"
#include <libtcc.h>
#include "lisp_types.h"
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

void with_symbols(var_def ** vars, size_t * vars_cnt, void (*fcn)()){

  push_symbols(vars,vars_cnt);
  fcn();
  pop_symbols();
}

var_def * get_variable(symbol name){

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

void basea2b(u8 (* read_glyph)(bool * ctn), u32 from_base, u32 to_base, void (*emit_glyph)(u8 glyph)){
  //buffer is a gigantic number that we stream the data into / from
  u32 buffer = 0;
  u32 basecount = 0;
  bool ctn = true;
  while(ctn){
    while(basecount < to_base && ctn){
      //read bits until we have a whole 'from' symbol
      buffer = (buffer * (from_base + 1)) + read_glyph(&ctn);
      basecount = basecount * (from_base + 1) + from_base;
      //logd(":: %i %i\n",basecount, to_base);
    }
    
    // emit the char
    emit_glyph(buffer % (to_base + 1));
    buffer /=  (to_base + 0);
    basecount /= (to_base + 0);
    //logd("::: %i\n",basecount);
  }
  emit_glyph(buffer);
}


char base61char(int i){
  if(i < 10)
    return '0' + i;
  if(i < 10 + 26)
    return 'A' + (i - 10);
  if(i < 10 + 26 + 26)
    return 'a' + (i - 10 - 26);
  return '_';
}

int ibase61char(char c){
  if(c >= '0' && c <= '9')
    return c - '0';
  if(c >= 'A' && c<= 'Z')
    return c - 'A' + 10;
  if(c >= 'a' && c <= 'z')
    return c - 'a' + 10 + 26;
  return 62; //'_'
}

void basen_encode(char * data, int len, int base, void (* newglyph)(int glyph)){
  int buffer = 0;
  int basecount = 0;
  for(int i = 0; i < len;){
    while(basecount < base){
      //read in a 
      buffer = buffer * 0xFF + data[i];
      basecount = (basecount * 0xFF) + 0xFF;
      i++;
    }
    // eat the first base bits
    newglyph(buffer % base);
    buffer /= base;
    basecount /= base;
  }
  newglyph(buffer % base);
}


void basen_decode(char * data, int len, int base, void (*newglyph)(int glyph)){
  int buffer = 0;
  int basecount = 0;
  for(int i = 0; i < len;){
    while(basecount < 0xFF){
      //read bits until we have a whole char
      buffer = (buffer * base) + data[i];
      basecount = basecount * base + base;
      i++;
    }
    // emit the char
    newglyph(buffer % 0xFF);
    buffer /=  0xFF;
    basecount /= 0xFF;
  }
  newglyph(buffer);
}

void base61_format(char * str){
  //{0-9, A-Z, a-z, _ }.
  int c = 0;
  int code = 0;
  while(*str != 0){
    c = (c << 8) | *str;
    //logd("C: %i\n",c);
    code = (code << 8) + 0xFF;
    while(code > 63){
      char c2 = base61char(c % 63);
      format("%c", c2);
      c = c / 63;
      code = code / 63;
    }
    str++;
  }
  char last = base61char(c);
  format("%c", last);
}

void base61_inverse_format(char * str){
  int nchar = 0;
  int bitcount = 0;
  while(*str != 0){
    bitcount = (bitcount << 8) + 0xFF;
    format("----\n");
    while(bitcount > 63 && *str != 0){
      int inv = ibase61char(*str);
      format("inv: %i %i %i\n", inv,  nchar, bitcount);
      nchar = nchar * 63 + inv;
      bitcount /= 63;
      str++;
    }
    
    format("c: %c %i %i\n", nchar, nchar, bitcount);
    nchar /= 63;
   
  }
}

void format_c_name(symbol s){
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
    if(first_alpha){
      format("%s", sym);
    }else{
      format("S_%s",sym);
    }
    return;
  }else{
    static i32 uniqueid = 0;
    static symbol_lookup * lut = NULL;
    symbol_lookup _lut_item;
    symbol_lookup * lut_item = &_lut_item;
    HASH_FIND_STR(lut, sym, lut_item);
    if(lut_item != NULL){
      format("%s", lut_item->cname);
      return;
    }
    lut_item = alloc0(sizeof(symbol_lookup));
    char * value = fmtstr("S__%i", uniqueid);

    lut_item->key = sym;
    lut_item->cname = value;
    HASH_ADD_KEYPTR(hh, lut, lut_item->key, strlen(lut_item->key), lut_item);
    uniqueid += 1;
    format("%s", value);
  }
}

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
    return fmtstr("S_%s",sym);
  }else{
    static i32 uniqueid = 0;
    static symbol_lookup * lut = NULL;
    symbol_lookup _lut_item;
    symbol_lookup * lut_item = &_lut_item;
    HASH_FIND_STR(lut, sym, lut_item);
    if(lut_item != NULL) return lut_item->cname;
    lut_item = alloc0(sizeof(symbol_lookup));
    char * value = fmtstr("S__%i", uniqueid);

    lut_item->key = sym;
    lut_item->cname = value;
    HASH_ADD_KEYPTR(hh, lut, lut_item->key, strlen(lut_item->key), lut_item);
    uniqueid += 1;
    return value;
  }
}


cmacro_def * get_cmacro_def(symbol s){
  var_def * var = get_variable(s);
  if(var == NULL){
    return NULL;
  }
  
  if(var->type != &cmacro_def_def){
    return NULL;
  }
  return (cmacro_def *) var->data;
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

  format("hello! :");
  base61_format("hello");
  format("\n");
  base61_format("+-_)@# hello cxz_-@#!%");

  format("\ninverse: ");
  
  base61_inverse_format("fg27Sq5"); 
  format("\n");

  format("\n Better test:\n");
  char * toencode = "llllllll";
  
  char * data = NULL;
  size_t data_cnt = 0;

  void print_base63glyph(int glyph){
    list_add((void **)&data,&data_cnt,&glyph, sizeof(char));
  }

  basen_encode(toencode, strlen(toencode), 63, print_base63glyph);

  char * out_data = NULL;
  size_t out_data_cnt = 0;

  void print_invbase63glyph(int glyph){
    list_add((void **)&out_data,&out_data_cnt,&glyph, sizeof(char));
  }

  basen_decode(data,data_cnt, 63, print_invbase63glyph);
  for(size_t i = 0; i < strlen("hello") + 1; i++){
    format("%i -> %i -> %i\n", toencode[i], data[i], out_data[i]);
  }
  format("\n");

  char * ptr = toencode;
 u8 next_glyph(bool * ctn){
    char next = *ptr;
    logd("> %i\n", next);
    *ctn = next != 0;
    ptr = ptr + 1;
    return next;
  }
  char buff[100];
  size_t buffcnt = 0;
  void emit_glyph(u8 glyph){
    
    buff[buffcnt] = glyph;//base61char(glyph);
    buffcnt++;
  }
  //logd("%i %i\n ", 0xFF * (0xFF + 1) + 0xFF, 0xFFFF);
  basea2b(next_glyph, 0xFF, 0xFFFF, emit_glyph);

  size_t it2 = 0;
  u8 next_glyph2(bool *ctn){
    *ctn = it2 != buffcnt;
    if(!*ctn) return 0;
    char glyph = buff[it2];
    char glyph2 = glyph;//ibase61char(glyph);
    //logd(">> %i\n", glyph2);
    it2++;
    return glyph2;
  }
  
  void emit_glyph2(u8 glyph){
    format("--> %c %i\n", glyph);
  }
  basea2b(next_glyph2, 0xFFFF, 0xFF, emit_glyph2);
  
  format("\n");


  return TEST_FAIL;
  return TEST_SUCCESS;
}

bool test_symbols(){
  TEST(test_get_cname);
  TEST(test_symbol_table);
  return TEST_SUCCESS;
}


