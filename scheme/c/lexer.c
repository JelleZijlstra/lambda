#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "lexer.h"
#include "scheme.h"

#define CREATE_BUFFER(initial_size) \
    size_t buffer_size = 64; \
    char *buffer = malloc(buffer_size * sizeof(char)); \
    size_t position = 0;

#define RESIZE_BUFFER() \
    position++; \
    if(position == buffer_size) { \
        buffer_size *= 2; \
        buffer = realloc(buffer, buffer_size); \
    }

#define ADD_TOKEN(token, value) \
    *tail_ptr = make_list_entry(token, value); \
    tail_ptr = &(*tail_ptr)->next;

scheme_list_entry *scheme_lex(FILE *input) {
    scheme_list_entry *head;
    scheme_list_entry **tail_ptr = &head;
    scheme_val null_value;
    null_value.list = NULL;
    while(true) {
        int c = fgetc(input);
        if(c == EOF) {
            ADD_TOKEN(T_EOF, null_value);
            break;
        } else if(isspace(c)) {
            continue;
        } else if(c == '(') {
            ADD_TOKEN(T_LEFT_PAREN, null_value);
        } else if(c == ')') {
            ADD_TOKEN(T_RIGHT_PAREN, null_value);
        } else if(c == '\'') {
            ADD_TOKEN(T_QUOTE, null_value);
        } else if(c == '.') {
            ADD_TOKEN(T_PERIOD, null_value);
        } else if(c == '"') {
            CREATE_BUFFER(64);
            bool escaped = false;
            while(true) {
                int c = fgetc(input);
                if(c == EOF) {
                    throw_error("EOF encountered while scanning string literal");
                }
                if(escaped) {
                    if(c == 'n') {
                        buffer[position] = '\n';
                    } else if(c == 't') {
                        buffer[position] = '\t';
                    } else {
                        buffer[position] = c;
                    }
                    escaped = false;
                } else {
                    if(c == '\\') {
                        escaped = true;
                    } else if(c == '"') {
                        buffer[position] = '\0';
                        break;
                    } else {
                        buffer[position] = c;
                    }
                }
                RESIZE_BUFFER();
            }
            scheme_val value;
            value.string = buffer;
            ADD_TOKEN(T_STRING, value);
        } else if(isdigit(c)) {
            CREATE_BUFFER(8);
            buffer[position] = c;
            position++;
            while(true) {
                int c = fgetc(input);
                if(c == EOF) {
                    throw_error("EOF encountered while scanning integer literal");
                }
                if(isdigit(c)) {
                    buffer[position] = c;
                } else {
                    buffer[position] = '\0';
                    ungetc(c, input);
                    break;
                }
                RESIZE_BUFFER();
            }
            scheme_val value;
            value.integer = strtol(buffer, NULL, 10);
            free(buffer);
            ADD_TOKEN(T_INT, value);
        } else if(c == ';') {
            // consume comments
            int c;
            while((c = fgetc(input)) != '\n');
        } else if(c == '#') {
            c = fgetc(input);
            scheme_val value;
            if(c == 't') {
                value.boolean = true;
            } else if(c == 'f') {
                value.boolean = false;
            } else {
                throw_error("Unexpected character in boolean literal");
            }
            ADD_TOKEN(T_BOOL, value);
        } else {
            CREATE_BUFFER(64);
            buffer[0] = c;
            position++;
            while(true) {
                int c = fgetc(input);
                if(c == '(' || c == ')' || c == EOF || isspace(c)) {
                    buffer[position] = '\0';
                    ungetc(c, input);
                    break;
                } else {
                    buffer[position] = c;
                }
                RESIZE_BUFFER();
            }
            scheme_val value;
            value.string = buffer;
            ADD_TOKEN(T_NAME, value);
        }
    }
    return head;
}

void free_token_list(scheme_list_entry *lst) {
    if(lst == NULL) {
        return;
    }
    switch(lst->tkn.kind) {
        case T_STRING: case T_NAME: free(lst->tkn.value.string); break;
        default: break;
    }
    scheme_list_entry *next = lst->next;
    free(lst);
    free_token_list(next);
}

void print_token_list(scheme_list_entry *head) {
    if(head == NULL) {
        return;
    }
    switch(head->tkn.kind) {
        case T_EOF: printf("T_EOF\n"); break;
        case T_STRING: printf("T_STRING: %s\n", head->tkn.value.string); break;
        case T_NAME: printf("T_NAME: %s\n", head->tkn.value.string); break;
        case T_INT: printf("T_INT: %d\n", head->tkn.value.integer); break;
        case T_BOOL: printf("T_BOOL: %s\n", (head->tkn.value.boolean ? "true" : "false")); break;
        case T_LEFT_PAREN: printf("T_LEFT_PAREN: (\n"); break;
        case T_RIGHT_PAREN: printf("T_RIGHT_PAREN: )\n"); break;
        case T_PERIOD: printf("T_PERIOD: .\n"); break;
        case T_QUOTE: printf("T_QUOTE: '\n"); break;
        case T_LIST: assert(false); /* Impossible */ break;
    }
    print_token_list(head->next);
}

