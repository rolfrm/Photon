#include<iron/full.h>
#include "uthash.h"
#include "lisp_parser.h"
#include "lisp_types.h"
type_def * str2type(char * str);

typedef struct {
  void ** key;
  size_t size;
  UT_hash_handle hh; 
}allocated_items;

allocated_items * arrays = NULL;

void ensure_size(void ** buffer, size_t min_size){
  allocated_items * item = NULL;
  HASH_FIND_PTR(arrays, &buffer, item);
  if(item == NULL){
    item = alloc0(sizeof(allocated_items));
    item->key = buffer;
    HASH_ADD_PTR(arrays, key, item);
  }
  if(item->size < min_size){
    *item->key = ralloc(*item->key, min_size);
    item->size = min_size;
  } 
}

void ensure_size2(void ** buffer, size_t * size, size_t min_size){
  if(size < min_size){
    *buffer = ralloc(*buffer, min_size);
    *size = min_size;
  } 
}
typedef struct{
  type_def * fcn_type;
  void * fcn;
  
}node_type;

typedef struct{
  node_type type;
  void (* destroy) (node_base * node);
}node_base;

typedef struct{
  node_base base;
  double * buffer;
  u64 buffer_size;
}norm2_data;

size_t calc_norm2(double * in_xes, double * in_yes, double * in_zes, size_t cnt, norm2_data * output){
  ensure_size2((void **) &output->buffer, &output->buffer_size, cnt * sizeof(double));
  double * o = output->buffer;
  for(size_t i = 0; i < cnt; i++){
    o[i] = sqrt(in_xes[i] * in_xes[i] + in_yes[i] * in_yes[i] + in_zes[i] * in_zes[i]);
  }
  return cnt;
}

typedef struct{
  node_base base;
  double * buffer;
  u64 buffer_size;
  double * delay_buffer;
  i64 delay_size;
}delay_data;

size_t calc_delay_line(double * in, size_t cnt, double ** out, delay_data * delay_buffer){
  ensure_size2((void **) &delay_buffer->buffer, &delay_buffer->buffer_size, cnt * sizeof(double));
  ensure_size2((void **) &delay_buffer->delay_buffer, &delay_buffer->delay_size, delay_buffer->delay_size);
  
  double * o = *out;
  for(size_t i = 0; i < cnt; i++){
    //??
    o[i] = delay_buffer->delay_buffer;
    **delay_buffer = in[i];
  }
  return cnt;
}

size_t copy_buffer(const double * in, size_t cnt, double ** out){
  ensure_size((void **)out, sizeof(double)* cnt);
  memcpy(*out, in, cnt * sizeof(double));
  return cnt;
}

size_t sine_generator(double frequency, double phase, size_t cnt, double ** out_buffer){
  ensure_size((void **) out_buffer, cnt * sizeof(double));
  for(size_t i = 0; i < cnt; i++){
    (*out_buffer)[i] = sin((double)i * frequency + phase);
  }
  return cnt;
} 

size_t add_line(const double * a, const double * b, size_t cnt, double ** o){
  ensure_size((void **) o, cnt * sizeof(double));
  for(size_t i = 0; i < cnt; i++)
    (*o)[i] = a[i] + b[i];
  return cnt;
}

void run_http_serv();
void load_defs();
bool test_dataflow(){
  load_defs();
  type_def * t = str2type("(fcn u64 (frequency f64) (phase f64) (cnt u64) (output (ptr (ptr f64))))");
  // this can be used to describe types to the system.
  
  run_http_serv();
  while(true)
    usleep(100000);
  logd("Testing dataflow..\n");
  double * x = NULL, * y = NULL, * z = NULL;
  double * r = NULL;
  double * r2 = NULL;
  double * r3 = NULL;
  size_t cnt = sine_generator(0.1, 0.0, 100, &x);
  sine_generator(0.1, 3.14/3, 100, &y);
  sine_generator(0.1, 2 * 3.14/3, 100, &z);
  
  //size_t r_size = calc_norm2(x, y, z, cnt, &r);
  add_line(x,y,cnt,&r);
  add_line(r,z,cnt,&r);
  calc_norm2(r, r, r, cnt, &r);
  calc_norm2(r, r, r, cnt, &r);
  for(int i = 0; i < 1; i++){
    calc_delay_line(r, cnt, &r2, &r3);
    copy_buffer(r2,cnt, &r);
  }
  for(size_t i = 0; i < cnt; i++){
    //logd("%f  %i\n", r2[i], r);
  }

  return true;
}
