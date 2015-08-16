OPT = -O4 -g0
SOURCES =  foton.c ../iron/linmath.c ../iron/utils.c lisp_types.c lisp_std_types.c ../iron/mem.c ../iron/fileio.c ../iron/array.c ../iron/math.c ../iron/time.c ../iron/hashtable.c ../iron/log.c  lisp_symbols.c lisp_compiler2.c lisp_compiler.c lisp_parser.c type_pool.c builtin_macros.c expr_utils.c c_ast.c builtin_functions.c #../iron/coroutines.c repl.c
CC = gcc
TARGET = foton
OBJECTS =$(SOURCES:.c=.o) 
LDFLAGS= -L. -L../libconcurrency-read-only/  $(OPT) -Wextra #-lmcheck #-ftlo  #setrlimit on linux 
LIBS= -lm -lpthread -lreadline -ldl

CFLAGS = -Itcc -I.. -I../libconcurrency-read-only/libconcurrency/ -std=c11 -c -Wall -Wextra -Werror=implicit-function-declaration -Wformat=0  $(OPT) -D_GNU_SOURCE -fdiagnostics-color -Werror

all: $(TARGET)
$(TARGET): $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) $(LIBS) tcc/libtcc.a libtcc1.a $(OPT) -o $@

.c.o: $(HEADERS)
	$(CC) $(CFLAGS) $< -o $@ -MMD -MF $@.depends
depend: h-depend
clean:
	rm $(OBJECTS) $(TARGET) *.o.depends
-include $(OBJECTS:.o=.o.depends)
