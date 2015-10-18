#include<iron/full.h>
#include "uthash.h"
#include "lisp_parser.h"
#include "lisp_types.h"
type_def * str2type(char * str);
#define swap_ptr(p1, p2) {void * t = p1; p1 = p2; p2 = t;}
typedef size_t id;
typedef void * userdata;
typedef struct{
  // fcn type
  type_def ** type;
  // update fcn (called when active)
  void ** update_fcn;
  // destructor
  void ** destroy_fcn;
  // initializer
  void ** init_fcn;
  
  // bit field of hot inputs.
  u64 * hot_inputs;
	  
  // number of inputs
  int * n_inputs;
  // number of outputs
  int * n_outputs;
  // number of userdatas
  int * n_userdatas;
  // if the node is reflective (2 extra args)
  bool * reflective;
  
  size_t cnt, capacity;
}node_definition;

typedef struct{
  // the type of this node
  id * type_id;
  // first output buffer in the list. last is this + n_outputs.
  size_t * output_buffers_start;
  // first userdata item
  size_t * userdata_start;
  // points to the output buffer that it can take its input from. 0 means no input.
  id * input_connections_start;
  
  size_t cnt, capacity;
}node;

typedef struct{
  userdata * buffers;
  size_t cnt, capacity;
}heap_buffer;

typedef struct{
  id * active_node_id;
  id * active_node_backbuffer;
  size_t cnt, capacity;
}active_node;

typedef struct{
  id * output_buffer;
  size_t cnt, capacity;
}input_connection;

typedef struct{
  id * output_node;
  id * input_node;
  size_t cnt, capacity;
}hot_connection;

typedef struct{
  node_definition node_definitions;
  node nodes;
  active_node active_nodes;
  input_connection connections;
  heap_buffer heap;
  hot_connection hot_connections;
}dataflow_context;
#define realloc_column(table,column,capacity) table->column = realloc(table->column,sizeof(table->column[0]) * capacity);

id add_node_def(node_definition * ctx, type_def * type, void * update_fcn, 
		int n_inputs, int n_outputs, int n_userdata, bool is_reflective){

  if(ctx->cnt == ctx->capacity){
    size_t newcap = 16;
    if(ctx->capacity != 0)
      newcap = ctx->capacity * 2;
    ctx->capacity = newcap;
    ctx->type = realloc(ctx->type, sizeof(void *) * newcap);
    ctx->update_fcn = realloc(ctx->update_fcn, sizeof(void *) * newcap);
    ctx->destroy_fcn = realloc(ctx->destroy_fcn, sizeof(void *) * newcap);
    ctx->init_fcn = realloc(ctx->init_fcn, sizeof(void *) * newcap);
    ctx->n_inputs = realloc(ctx->n_inputs, sizeof(int) * newcap);
    ctx->n_outputs = realloc(ctx->n_outputs, sizeof(int) * newcap);
    ctx->n_userdatas = realloc(ctx->n_userdatas, sizeof(int) * newcap);
    ctx->reflective = realloc(ctx->reflective, sizeof(bool) * newcap);
    realloc_column(ctx, hot_inputs, newcap);
  }
  size_t i = ctx->cnt++;
  ctx->update_fcn[i] = update_fcn;
  ctx->type[i] = type;
  ctx->n_inputs[i] = n_inputs;
  ctx->n_outputs[i] = n_outputs;
  ctx->n_userdatas[i] = n_userdata;
  ctx->reflective[i] = is_reflective;
  ctx->hot_inputs[i] = 0;
  return i;
}

size_t alloc_heap_buffer(heap_buffer * ctx, size_t cnt){
  if(ctx->cnt == ctx->capacity){
    size_t newcap = 16;
    if(ctx->capacity != 0)
      newcap = ctx->capacity * 2;
    ctx->capacity = newcap;
    realloc_column(ctx, buffers, newcap);
  }
  size_t i = ctx->cnt;
  ctx->cnt += cnt;
  return i;
}

id alloc_input_connections(input_connection * ctx, size_t cnt){
  if(ctx->cnt == ctx->capacity){
    size_t newcap = 16;
    if(ctx->capacity != 0)
      newcap = ctx->capacity * 2;
    ctx->capacity = newcap;
    realloc_column(ctx, output_buffer, newcap);
  }
  size_t i = ctx->cnt;
  ctx->cnt += cnt;
  return i;
}

id alloc_hot_connection(hot_connection * table, id output_node, id input_node){
  if(table->cnt == table->capacity){
    size_t newcap = 16;
    if(table->capacity != 0)
      newcap = table->capacity * 2;
    table->capacity = newcap;
    realloc_column(table, output_node, newcap);
    realloc_column(table, input_node, newcap);
  }
  size_t i = table->cnt++;
  table->output_node[i] = output_node;
  table->input_node[i] = input_node;
  return i;
}
	  
id add_node(dataflow_context * ctx, id node_type_id){
  node_definition * nd = &ctx->node_definitions;
  node * n = &ctx->nodes;
  if(n->cnt == n->capacity){
    size_t newcap = 16;
    if(n->capacity != 0)
      newcap = nd->capacity * 2;
    n->capacity = newcap;
    realloc_column(n, type_id, newcap);
    realloc_column(n, output_buffers_start, newcap);
    realloc_column(n, userdata_start, newcap);
    realloc_column(n, input_connections_start, newcap); 
  }
  size_t i = n->cnt++;
  n->type_id[i] = node_type_id;
  n->output_buffers_start[i] = alloc_heap_buffer(&ctx->heap, nd->n_outputs[node_type_id]);
  n->userdata_start[i] = alloc_heap_buffer(&ctx->heap, nd->n_userdatas[node_type_id]);
  n->input_connections_start[i] = alloc_input_connections(&ctx->connections, nd->n_inputs[node_type_id]);
  
  return i;
}

size_t push_active(active_node * ctx, id node){
  if(ctx->cnt == ctx->capacity){
    size_t newcap = 16;
    if(ctx->capacity != 0)
      newcap = ctx->capacity * 2;
    realloc_column(ctx, active_node_id, newcap);
    realloc_column(ctx, active_node_backbuffer, newcap);
  }
  size_t i = ctx->cnt++;
  ctx->active_node_id[i] = node;
  return i;
}
	  
void connect_nodes(dataflow_context * ctx, id output_node, size_t output_index, id input_node, size_t input_index){
  id input_offset = ctx->nodes.input_connections_start[input_node] + input_index;
  id output_offset = ctx->nodes.output_buffers_start[output_node] + output_index;
  id input_type = ctx->nodes.type_id[input_node];
  u64 hot_input_conf = ctx->node_definitions.hot_inputs[input_type];
  if(hot_input_conf & (1 << input_index))
    alloc_hot_connection(&ctx->hot_connections, output_node, input_node);
  ctx->connections.output_buffer[input_offset] = output_offset;
}
	  
int compare( const void* a, const void* b)
{
     id int_a = * ( (id*) a );
     id int_b = * ( (id*) b );

     if ( int_a == int_b ) return 0;
     else if ( int_a > int_b ) return -1;
     else return 1;
}
	  
int compare2(const void * a, const void * b){
     id int_a = * ( (id*) a );
     id int_b = * ( (id*) b );
     return int_a == int_b;
}

size_t distinct_sorted(void * array, size_t cnt, size_t elem_size, int (* cmp)(const  void * a, const void * b)){
  if(cnt == 0) return 0;
  size_t current = 0;
  size_t current_index = 0;
  
  for(size_t i = 0; i < cnt; i++){
    size_t index = i * elem_size;
    if(!cmp(array + current_index, array + index)){
      current += 1;
      current_index = current * elem_size;
      memcpy(array + current_index, array + index, elem_size);
    }
  }
  return current + 1;
}


void update_nodes(dataflow_context * ctx){
  if(ctx->active_nodes.cnt == 0) return;
  qsort(ctx->active_nodes.active_node_id, ctx->active_nodes.cnt, sizeof(ctx->active_nodes.active_node_id[0]), compare);
  id * ids = ctx->active_nodes.active_node_id;
  size_t unique_cnt = distinct_sorted(ctx->active_nodes.active_node_id, ctx->active_nodes.cnt, sizeof(ctx->active_nodes.active_node_id[0]), compare2);
  swap_ptr(ctx->active_nodes.active_node_id, ctx->active_nodes.active_node_backbuffer);
  ctx->active_nodes.cnt = 0;  
  for(size_t i= 0; i < unique_cnt; i++){
    
    id n_id = ids[i];
    id t_id = ctx->nodes.type_id[n_id];
    bool is_reflective = ctx->node_definitions.reflective[t_id];
    size_t n_inputs = ctx->node_definitions.n_inputs[t_id];
    size_t n_userdatas = ctx->node_definitions.n_userdatas[t_id];
    size_t n_outputs = ctx->node_definitions.n_outputs[t_id];
    size_t total_inputs = n_inputs + n_outputs + n_userdatas + (is_reflective ? 0 : 2);
    void * args[total_inputs];
    void ** pargs = args;
    if(is_reflective){
      pargs[0] = ctx;
      pargs[1] = (void *) n_id;
      pargs += 2;
    }
    {
      id userdatas_start = ctx->nodes.userdata_start[n_id];
      for(size_t j = 0; j < n_userdatas; j++){
	pargs[j] = ctx->heap.buffers[userdatas_start + j];
      }
      pargs += n_userdatas;
    }
    {
      id con_start = ctx->nodes.input_connections_start[n_id];
      for(size_t j = 0; j < n_inputs; j++){

	id con_this = con_start + j;
	id conn = ctx->connections.output_buffer[con_this];
	pargs[j] = &ctx->heap.buffers[conn];
      }
      pargs += n_inputs;
    }
    {
      size_t output_start = ctx->nodes.output_buffers_start[n_id];
      for(size_t j = 0; j < n_outputs; j++){
	pargs[j] = &ctx->heap.buffers[output_start + j];
      }
      pargs += n_outputs;
    }
    void (* fcn)(void * v, ...)  = ctx->node_definitions.update_fcn[t_id];

    switch(total_inputs){
    case 1: fcn(args[0]); break;
    case 2: fcn(args[0], args[1]); break;
    case 3: fcn(args[0], args[1], args[2]); break;
    case 4: fcn(args[0], args[1], args[2], args[3]); break;
    case 5: fcn(args[0], args[1], args[2], args[3], args[4]); break;
    case 6: fcn(args[0], args[1], args[2], args[3], args[4], args[5]); break;
    case 7: fcn(args[0], args[1], args[2], args[3], args[4], args[5], args[6]); break;
    default: ERROR("Unsupported number of args");break;
    } 
  }
  for(size_t i= 0; i < unique_cnt; i++){
    for(size_t j = 0; j < ctx->hot_connections.cnt; j++)
      if(ctx->hot_connections.output_node[j] == ids[i]){
	push_active(&ctx->active_nodes, ctx->hot_connections.input_node[j]);
      }
  }
}
	  


static void random_value(f32 * out){
  *out = 3.14;
}
	  
static void add_values(f32 * a, f32 * b, f32 * result){
  *result = *a + *b;
}

static void print_value(f32 * in){
  logd("Value: %f\n", *in);
}
int compare3( const void* a, const void* b)
{
     int int_a = * ( (int*) a );
     int int_b = * ( (int*) b );

     if ( int_a == int_b ) return 0;
     else if ( int_a < int_b ) return -1;
     else return 1;
}
int compare4( const void* a, const void* b)
{
     int int_a = * ( (int*) a );
     int int_b = * ( (int*) b );
     return int_a == int_b;
}

bool qsort_test(){
  int values[] = {10,4,2,6,2,2,3,5,1,1,4,6,10,2};
  qsort(values, array_count(values),sizeof(int), compare3);
  for(size_t i = 0; i < array_count(values); i++)
    logd("Sorted? %i\n", values[i]);
  size_t distinct_cnt = distinct_sorted(values,array_count(values), sizeof(values[0]), compare4);
  for(size_t i = 0; i < distinct_cnt; i++)
    logd("distinct? %i\n", values[i]);
  return true;	  
} 
	  
void load_defs();
bool test_dataflow(){
  load_defs();
  dataflow_context ctx;
  memset(&ctx, 0, sizeof(ctx));
  id t1 = add_node_def(&ctx.node_definitions, 
		       str2type("(fcn void (out (ptr f64)))"), random_value,
		       0,1,0,0);
  id t2 = add_node_def(&ctx.node_definitions, 
		       str2type("(fcn void (in f64))"), print_value,
		       1,0,0,0);
  id t3 = add_node_def(&ctx.node_definitions, 
		       str2type("(fcn void (in f64) (in2 f64) (in3 f64))"), add_values,
		       2,1,0,0);
  ctx.node_definitions.hot_inputs[t2] = 0b1;
  ctx.node_definitions.hot_inputs[t3] = 0b11;
  id c1 = add_node(&ctx, t1);
  id c2 = add_node(&ctx, t1);
  id n2 = add_node(&ctx, t2);
  id print2 = add_node(&ctx, t2);
  id print3 = add_node(&ctx, t2);
  id print4 = add_node(&ctx, t2);
  id add = add_node(&ctx, t3);
  id add2 = add_node(&ctx, t3);


  connect_nodes(&ctx, c1, 0, add, 0);
  connect_nodes(&ctx, c2, 0, add, 1);
  connect_nodes(&ctx, add, 0, n2, 0);
  connect_nodes(&ctx, add, 0, print2, 0);
  connect_nodes(&ctx, add, 0, print3, 0);
  connect_nodes(&ctx, add, 0, add2, 0);
  connect_nodes(&ctx, add2, 0, add2, 1);
  connect_nodes(&ctx, add2, 0, print4, 0);

  //UNUSED(print2);
  push_active(&ctx.active_nodes, c1);
  push_active(&ctx.active_nodes, c2);
  update_nodes(&ctx);
  update_nodes(&ctx);
  logd("p: %p\n", ctx.active_nodes.active_node_id[0]);
  for(int i = 0; i < 20;i++){
    update_nodes(&ctx);
  }

  
  return true;
}
