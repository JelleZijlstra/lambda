
void throw_error(const char *message);

scheme_list_entry *make_list_entry(token_t kind, scheme_val value);

scheme_list_entry *copy_list_entry(scheme_list_entry *old_entry);
