#ifndef LEXER_H_
#define LEXER_H_

#include <stdbool.h>

typedef enum token_t {
    T_EOF, T_STRING, T_NAME, T_INT, T_BOOL, T_LEFT_PAREN, T_RIGHT_PAREN, T_PERIOD, T_QUOTE, T_LIST, T_LIB_FUNCTION, T_LIB_MACRO
} token_t;

struct scheme_list_entry;
struct scheme_context;

typedef scheme_list_entry *scheme_library_function(scheme_list_entry *);
typedef scheme_list_entry *scheme_library_macro(scheme_list_entry *, scheme_context*);

typedef union scheme_val {
    char *string;
    int integer;
    bool boolean;
    void *list;
    scheme_library_function *library_function;
} scheme_val;

typedef struct labeled_val {
    token_t kind;
    scheme_val value;
} labeled_val;

typedef struct scheme_list_entry {
    labeled_val tkn;
    struct scheme_list_entry *next;
} scheme_list_entry;

scheme_list_entry *scheme_lex(FILE *input);

void free_token_list(scheme_list_entry *lst);

void print_token_list(scheme_list_entry *head);

#endif /* LEXER_H_ */
