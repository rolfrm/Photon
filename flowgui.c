
#include <microhttpd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <iron/utils.h>
#include <iron/log.h>
#include <iron/fileio.h>

static int ahc_echo(void * cls,
		    struct MHD_Connection * connection,
		    const char * url,
		    const char * method,
                    const char * version,
		    const char * upload_data,
		    size_t * upload_data_size,
                    void ** ptr) {
  UNUSED(url); UNUSED(version); UNUSED(upload_data); UNUSED(upload_data_size);
  UNUSED(method); UNUSED(cls);
  //logd("Got: %s %s %s %s page:%s\n", url, method, version, upload_data, cls);
  //static int dummy;
  

  struct MHD_Response * response;
  int ret;

  *ptr = NULL; /* clear context pointer */
  if(url[1] == 't'){
    double data[] = {1,2,3.23,4,5};
    response = MHD_create_response_from_data(5 * 8,
					     (void*) data,
					     0,
					     MHD_NO);
  }else{
    char * page =   page = read_file_to_string("page.html");;
    response = MHD_create_response_from_data(strlen(page),
					     (void*) page,
					     1,
					     MHD_NO);
  }
  ret = MHD_queue_response(connection,
			   MHD_HTTP_OK,
			   response);

  MHD_destroy_response(response);
  return ret;
}

void run_http_serv() {
  struct MHD_Daemon * d;
  UNUSED(d);

  d = MHD_start_daemon(MHD_USE_THREAD_PER_CONNECTION,
		       8000,
		       NULL,
		       NULL,
		       &ahc_echo,
		       NULL,
		       MHD_OPTION_END);
}
