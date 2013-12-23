#include "scheme.h"

typedef struct scheme_context {
    hash_table *names;
    hash_table *macros;
    struct scheme_context *meval_context;
} scheme_context;


scheme_list_entry *scheme_eval(scheme_list_entry *);
