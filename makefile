OPT = -g
SOURCES =  foton.c ../iron/linmath.c ../iron/utils.c lisp_types.c lisp_std_types.c ../iron/mem.c ../iron/fileio.c ../iron/array.c ../iron/math.c ../iron/time.c ../iron/hashtable.c ../iron/log.c  lisp_symbols.c lisp_compiler2.c lisp_compiler.c lisp_parser.c repl.c
CC = gcc
TARGET = foton
OBJECTS =$(SOURCES:.c=.o) 
LDFLAGS=-ldl -L. -L../libconcurrency-read-only/  $(OPT) -Wextra #-lmcheck #-ftlo  #setrlimit on linux 
LIBS= -ldl -lm -lpthread

CFLAGS = -Itcc -I.. -I../libconcurrency-read-only/libconcurrency/ -std=c11 -c $(OPT) -Wall -Wextra -Werror=implicit-function-declaration -Wformat=0  -g3 -O0 -D_GNU_SOURCE -fdiagnostics-color -Werror

all: $(TARGET)
$(TARGET): $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) $(LIBS) tcc/libtcc.a -ldl -o $@

.c.o: $(HEADERS)
	$(CC) $(CFLAGS) $< -o $@ -MMD -MF $@.depends
depend: h-depend
clean:
	rm $(OBJECTS) $(TARGET) *.o.depends
-include $(OBJECTS:.o=.o.depends)
