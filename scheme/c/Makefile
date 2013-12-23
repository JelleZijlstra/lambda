CC = clang
CCFLAGS = -std=c99 -Wall -pedantic -Wextra -ggdb
SOURCES = lexer.c main.c parser.c hash_table.c
OBJECTS = $(SOURCES:.c=.o)
EXECUTABLE = cscheme

all: $(EXECUTABLE)

%.o: %.c
	$(CC) -c $(CCFLAGS) $< -o $@

$(EXECUTABLE): $(OBJECTS)
	$(CC) $(OBJECTS) -o $(EXECUTABLE)

clean:
	rm $(EXECUTABLE) *.o
