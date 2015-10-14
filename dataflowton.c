#include<iron/full.h>
#include "uthash.h"

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

size_t calc_norm2(double * in_xes, double * in_yes, double * in_zes, size_t cnt, double ** output){
  ensure_size((void **) output, cnt * sizeof(double));
  double * o = *output;
  for(size_t i = 0; i < cnt; i++){
    o[i] = sqrt(in_xes[i] * in_xes[i] + in_yes[i] * in_yes[i] + in_zes[i] * in_zes[i]);
    logd("%f\n", o[i]);
  }
  return cnt;
}

size_t calc_delay_line(double * in, size_t cnt, double ** out, double ** delay_buffer){
  ensure_size((void **) out, cnt * sizeof(double));
  if(*delay_buffer == NULL)
    *delay_buffer = alloc0(sizeof(double));
  double * o = *out;
  for(size_t i = 0; i < cnt; i++){
    o[i] = **delay_buffer;
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

bool test_dataflow(){
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
