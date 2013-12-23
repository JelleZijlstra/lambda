struct hash_table;

typedef struct hash_table hash_table;

hash_table *make_hash_table();

void hash_table_set(hash_table *, const char *, void *);

void *hash_table_get(hash_table *, const char *);

void destroy_hash_table(hash_table *);
