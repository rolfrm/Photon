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

size_t copy_buffer(double *in, size_t cnt, double ** out){
  ensure_size((void **)out, sizeof(double)* cnt);
  memcpy(*out, in, cnt * sizeof(double));
  return cnt;
}


void run_http_serv();
bool test_dataflow(){
  run_http_serv();
  while(true)
    usleep(100000);
  logd("Testing dataflow..\n");
  double x[10], y[10], z[10];
  double * r = NULL;
  double * r2 = NULL;
  double * r3 = NULL;
  
  for(size_t i = 0; i < array_count(z); i++){
    x[i] = i & 1;
    y[i] = i & 3;
    z[i] = i ^ 4;
  }

  size_t r_size = calc_norm2(x, y, z, array_count(z), &r);
  for(int i = 0; i < 5; i++){
    calc_delay_line(r, r_size, &r2, &r3);
    copy_buffer(r2,r_size, &r);
  }
  for(size_t i = 0; i < r_size; i++){
    logd("%f  %i\n", r2[i], r);
  }

  return true;
}
