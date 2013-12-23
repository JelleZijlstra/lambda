#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

#include "parser.h"
#include "scheme.h"

static scheme_list_entry *do_parse(scheme_list_entry **input);

static scheme_list_entry *do_parse_list(scheme_list_entry **input) {
    scheme_list_entry *head;
    scheme_list_entry **tail_ptr = &head;
    bool in_list = true;
    while(in_list) {
        switch((*input)->tkn.kind) {
            case T_RIGHT_PAREN:
                *input = (*input)->next;
                in_list = false;
                break;
            case T_EOF:
                throw_error("Unexpected EOF within list");
            case T_PERIOD: {
                *input = (*input)->next;
                scheme_list_entry *rest = do_parse(input);
                switch(rest->tkn.kind) {
                    case T_NAME: {
                        scheme_val value;
                        value.string = rest->tkn.value.string;
                        scheme_list_entry *new_entry = make_list_entry(T_PERIOD, value);
                        *tail_ptr = new_entry;
                        tail_ptr = &new_entry->next;
                        free(rest);
                        break;
                    }
                    case T_LIST: {
                        scheme_list_entry *lst = rest->tkn.value.list;
                        while(lst != NULL) {
                            tail_ptr = &(lst->next);
                            lst = lst->next;
                        }
                        free(rest);
                        break;
                    }
                    default:
                        throw_error("Unexpected expression following .");
                }
                if((*input)->tkn.kind != T_RIGHT_PAREN) {
                    throw_error("Expected ) following . expression");
                }
                break;
            }
            default: {
                scheme_list_entry *lst_entry = do_parse(input);
                *tail_ptr = lst_entry;
                tail_ptr = &lst_entry->next;
                break;
            }

        }
    }
    scheme_val value;
    value.list = head;
    return make_list_entry(T_LIST, value);
}

static scheme_list_entry *do_parse(scheme_list_entry **input) {
    switch((*input)->tkn.kind) {
        case T_QUOTE: {
            *input = (*input)->next;
            scheme_val value;
            value.list = do_parse(input);
            return make_list_entry(T_QUOTE, value);
        }
        case T_LEFT_PAREN: {
            *input = (*input)->next;
            return do_parse_list(input);
        }
        case T_RIGHT_PAREN:
            throw_error("Unexpected )");
        case T_EOF:
            throw_error("Unexpected EOF");
        default: {
            scheme_list_entry *new_entry = copy_list_entry(*input);
            *input = (*input)->next;
            return new_entry;
        }
    }
}

scheme_list_entry *scheme_parse(scheme_list_entry *input) {
    scheme_list_entry *head;
    scheme_list_entry **tail_ptr = &head;
    while(true) {
        if(input->tkn.kind == T_EOF) {
            break;
        }
        *tail_ptr = do_parse(&input);
        tail_ptr = &(*tail_ptr)->next;
    }
    return head;
}

static void printtabs(int n) {
    for(int i = 0; i < n; i++) {
        putchar('\t');
    }
}

void print_parse_tree(scheme_list_entry *tree, int depth) {
    if(tree == NULL) {
        return;
    }
    printtabs(depth);
    switch(tree->tkn.kind) {
        case T_STRING: printf("T_STRING: %s\n", tree->tkn.value.string); break;
        case T_NAME: printf("T_NAME: %s\n", tree->tkn.value.string); break;
        case T_INT: printf("T_INT: %d\n", tree->tkn.value.integer); break;
        case T_BOOL: printf("T_BOOL: %s\n", (tree->tkn.value.boolean ? "true" : "false")); break;
        case T_LEFT_PAREN: printf("T_LEFT_PAREN: (\n"); break;
        case T_RIGHT_PAREN: printf("T_RIGHT_PAREN: )\n"); break;
        case T_PERIOD: printf("T_PERIOD: %s\n", tree->tkn.value.string); break;
        case T_QUOTE:
            printf("T_QUOTE:\n");
            print_parse_tree(tree->tkn.value.list, depth + 1);
            break;
        case T_LIST:
            printf("T_LIST:\n");
            print_parse_tree(tree->tkn.value.list, depth + 1);
            break;
        case T_EOF: assert(false); /* Impossible */ break;
    }
    print_parse_tree(tree->next, depth);
}

