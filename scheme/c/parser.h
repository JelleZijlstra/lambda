#ifndef PARSER_H_
#define PARSER_H_

#include "lexer.h"

scheme_list_entry *scheme_parse(scheme_list_entry *input);

void print_parse_tree(scheme_list_entry *tree, int depth);

#endif /* PARSER_H_ */
