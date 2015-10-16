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
  if(*size < min_size){
    *buffer = ralloc(*buffer, min_size);
    *size = min_size;
  } 
}
typedef struct{
  type_def * fcn_type;
  void * fcn;
  void (* destroy) (void * node, void * destroy_userdata);  
  int * inputs;
  bool * hot_inputs;
  int * outputs;
}node_type;

typedef struct{
  node_type type;
  void * userdata;
  int activity;
  void ** output_buffers;
}node_base;

typedef struct{
  node_base base;
  double * buffer;
  u64 buffer_size;
}norm2_data;

typedef struct{
  node_base base;
  u64 value;
}value_node_u64;
	  
size_t value_node_u64_fcn(value_node_u64 * node, u64 * output){
  *output = node->value;
  return 1;
}
	  
typedef struct{
  node_base base;
  void (* fcn) (u64 value);
}callback_u64_node;

size_t callback_u64(callback_u64_node * node, u64 value){
  node->fcn(value);
  return 0;
}
	  
size_t calc_norm2(norm2_data * output, double * in_xes, double * in_yes, double * in_zes, size_t cnt, double ** output_buffer){
  ensure_size2((void **) &output->buffer, &output->buffer_size, cnt * sizeof(double));
  double * o = output->buffer;
  for(size_t i = 0; i < cnt; i++){
    o[i] = sqrt(in_xes[i] * in_xes[i] + in_yes[i] * in_yes[i] + in_zes[i] * in_zes[i]);
  }
  *output_buffer = output->buffer;
  return cnt;
}
typedef struct{
  node_base base;
  double * buffer;
  u64 buffer_size;
}array_node;
u64 array_node(array_node *node, double ** output_buffer){
  *output_buffer = node->buffer;
  return node->buffer_size;
}
/*
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
  }*/

#include <stdarg.h>
typedef struct{
  size_t node_id;
  size_t local_index;
  size_t remote_index;
}connection;

typedef struct{
  connection * inputs;
  size_t input_cnt;
  connection * outputs;
  size_t output_cnt;
}node_connections;

typedef struct{
  node_connections ** connections;
  node_base ** nodes;
  size_t node_cnt;
}connections;

void update_nodes(connections * con){
  for(size_t i = 0; i < con->node_cnt; i++){
    node_base * node = con->nodes[i];
    if(node->activity){
      node->activity = 0;
      int input_cnt = 0, output_cnt = 0;
      for(int * inputs = node->node_type.inputs; *inputs != 0; inputs++)
	input_cnt += 1;
      for(int * outputs = node->node_type.outputs; *outputs != 0; outputs++)
	output_cnt += 1;      
      void * outputs[output_cnt];
      void * args[input_cnt + output_cnt];
      for(int i = 0; i < output_cnt; i++)
	args[i + input_cnt] = outputs + i;
      node_connections * ncon = con->connections[i];
      for(size_t i = 0; i < ncon->input_cnt; i++){
	connection * connection = ncon->inputs + i;
	args[i] = con->nodes[connection->node_id].output_buffers[connection->remote_index];
      }
      u64 (fcn *) (node_base * n, ...) = node->type.fcn;
      switch(output_cnt + input_cnt){
      case 0:
	fcn(node); break;
      case 1:
	fcn(node, args[0]); break;
      case 2:
	fcn(node, args[0], args[1]); break;
      case 3:
	fcn(node, args[0], args[1], args[2]); break;
      case 4:
	fcn(node, args[0], args[1], args[2], args[3]); break;
      case 5:
	fcn(node, args[0], args[1], args[2], args[3], args[4]); break;
      case 6:
	fcn(node, args[0], args[1], args[2], args[3], args[4], args[5]); break;
      case 7:
	fcn(node, args[0], args[1], args[2], args[3], args[4], args[5], args[6]); break;
      case 8:
	fcn(node, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]); break;
      }
      for(size_t i = 0; i < ncon->output_cnt; i++){
	connection connection = ncon->inputs[i];
	if(con->nodes[connection.node_id].type.hot_inputs[connection.remote_index]){
	  con->nodes[connection.node_id].activity++;
	}
      }
    }
  }
}

size_t ensure_node_con(connections * con, node_base * node){
  for(size_t i = 0; i < con->node_cnt; i++)
    if(con->nodes[i] == node)
      return i;
  size_t ocnt = con->node_cnt++;
  con->connections = realloc(con->connections, sizeof(node_connections *) * con->node_cnt);
  con->nodes = realloc(con->nodes, sizeof(node_connections *) * con->node_cnt);
  con->nodes[ocnt] = node;
  node_connections * new_con = alloc0(node_connections);
  con->connections[ocnt] = new_con;
  return ocnt;
}

void add_connection(connection ** con, size_t * size, size_t node_id, size_t local_index, size_t remote_ndex){
  size_t old_size = (*size)++;
  *con = realloc(*con, *size * sizeof(connection));
  (*con)[old_size].node_id = node_id;
  (*con)[old_size].local_index = local_index;
  (*con)[old_size].remote_index = remote_index;
}


void connect_nodes(connections * con, node_base * output, size_t output_index, 
		   node_base * input, size_t input_index){
  size_t input_id = ensure_node_con(con, input);
  size_t output_id = ensure_node_con(con, output);
  add_connection(&con->connections[input_id].inputs, &con->connections[input_id].input_cnt, output_id, output_index, input_index);
  add_connection(&con->connections[output_id].outputs, &con->connections[output_id].output_cnt, input_id, input_index, output_index);

}

void run_http_serv();
void load_defs();
bool test_dataflow(){
  load_defs();
  // this can be used to describe types to the system.
  norm2_data n2;
  memset(&n2,0,sizeof(n2));
  n2.base.type.fcn_type = str2type("(fcn u64 (user_data (ptr void)) (xes (ptr f64)) (yes (ptr f64)) (zes (ptr f64)) (cnt u64) (result (ptr f64)))");
  n2.base.type.fcn = calc_norm2;
  {
    int inputs [] = {1, 2, 3, 4, 0};
    int outputs[] = {5, 0};
    bool hot_inputs[] = {true, true, true, false};
    n2.base.type.inputs = inputs;
    n2.base.type.outputs = outputs;
    n2.base.type.hot_inputs = hot_inputs;
  }
  double * result_buffer = NULL;
  
  int empty_con[] = {0};
  value_node_u64 count_node;
  {
    int outputs [] = {1, 0};
    bool hot_inputs[] = {false};
    count_node.base.type.fcn_type = str2type("(fcn u64 (user_data (ptr void)) (output (ptr u64)))");
    count_node.base.type.inputs = empty_con;
    count_node.base.type.hot_inputs = hot_inputs;
    count_node.base.type.outputs = outputs;
    count_node.base.output_buffers = alloc0(sizeof(void *));
  }
  callback_node_u64 callback_node;
  {
    callback_node.base.type.fcn_type = str2type("(fcn u64 (user_data (ptr void)) (input u64))");
  }
  
  
  //fcn((node_base *) &n2, values, values, values, array_count(values), &result_buffer);
  
  logd("Done.. %f\n", result_buffer[2]);
  run_http_serv();
  while(true)
    usleep(100000);
  /*logd("Testing dataflow..\n");
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
  */
  return true;
}
