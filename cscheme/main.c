#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "lexer.h"
#include "parser.h"

void throw_error(const char *message) {
    fprintf(stderr, "%s\n", message);
    exit(1);
}

scheme_list_entry *make_list_entry(token_t kind, scheme_val value) {
    scheme_list_entry *out = malloc(sizeof(scheme_list_entry));
    out->tkn.kind = kind;
    out->tkn.value = value;
    out->next = NULL;
    return out;
}

scheme_list_entry *copy_list_entry(scheme_list_entry *old_entry) {
    scheme_val new_value;
    switch(old_entry->tkn.kind) {
        case T_STRING: case T_NAME:
            new_value.string = strdup(old_entry->tkn.value.string);
            break;
        case T_INT:
            new_value.integer = old_entry->tkn.value.integer;
            break;
        case T_BOOL:
            new_value.boolean = old_entry->tkn.value.boolean;
            break;
        case T_EOF: case T_LEFT_PAREN: case T_RIGHT_PAREN: case T_PERIOD: case T_QUOTE: case T_LIST:
            new_value.list = old_entry->tkn.value.list;
            break;
    }
    return make_list_entry(old_entry->tkn.kind, new_value);
}

int main(int argc, char *argv[]) {
    if(argc == 2) {
        FILE *fd = fopen(argv[1], "r");
        scheme_list_entry *lst = scheme_lex(fd);
        print_token_list(lst);
        scheme_list_entry *parse_tree = scheme_parse(lst);
        free_token_list(lst);
        print_parse_tree(parse_tree, 0);
    }
}
