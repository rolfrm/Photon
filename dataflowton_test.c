#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdarg.h>
#include <iron/types.h>
#include <iron/log.h>
#include <iron/test.h>
#include <iron/mem.h>

void _error(const char * file, int line, const char * str, ...){
  logd("Error at '%s':%i %s ", file, line, str);
}
bool test_dataflow();

int main(){
    test_dataflow();
    return 0;
}
