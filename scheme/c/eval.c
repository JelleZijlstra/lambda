#include "hash_table.h"
#include "scheme.h"

static scheme_list_entry *eval(scheme_list_entry *, scheme_context *);

scheme_list_entry *scheme_eval(scheme_list_entry *program) {
    scheme_context *env = new_context();
    return eval(program, env);
}

static scheme_list_entry *eval(scheme_list_entry *value, scheme_context *context) {
    switch(value->tkn.kind) {
        case T_STRING:
        case T_INT:
        case T_BOOL:
        case T_LIB_FUNCTION:
        case T_LIB_MACRO:
            return value;
        case T_NAME:
            // TODO
    }
}
