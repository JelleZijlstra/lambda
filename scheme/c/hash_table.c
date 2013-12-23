#include <stdlib.h>
#include <string.h>

#define INITIAL_SIZE 16

typedef struct bucket {
    const char *key;
    void *value;
    int hash;
    struct bucket *next;
} bucket;

typedef struct hash_table {
    int size;
    int occupied;
    bucket **table;
} hash_table;

static int hash(const char *key);

static bucket *get_bucket(hash_table *table, const char *key);

static void resize_table(hash_table *table);

hash_table *make_hash_table() {
    hash_table *out = malloc(sizeof(hash_table));
    out->size = INITIAL_SIZE;
    out->occupied = 0;
    out->table = calloc(INITIAL_SIZE, sizeof(bucket *));
    return out;
}

void hash_table_set(hash_table *table, const char *key, void *value) {
    bucket *bkt = get_bucket(table, key);
    if(bkt != NULL) {
        bkt->value = value;
        return;
    }
    if(table->occupied + 1 > table->size / 2) {
        resize_table(table);
    }
    bucket *new_bkt = malloc(sizeof(bucket));
    new_bkt->key = key;
    new_bkt->value = value;
    new_bkt->hash = hash(key);
    int index = new_bkt->hash % table->size;
    new_bkt->next = table->table[index];
    table->table[index] = new_bkt;
    table->occupied++;
}

void *hash_table_get(hash_table *table, const char *key) {
    bucket *bkt = get_bucket(table, key);
    return bkt == NULL ? NULL : bkt->value;
}

static bucket *get_bucket(hash_table *table, const char *key) {
    int key_hash = hash(key);
    for(bucket *bkt = table->table[key_hash % table->size]; bkt != NULL; bkt = bkt->next) {
        if(bkt->hash == key_hash && !strcmp(bkt->key, key)) {
            return bkt;
        }
    }
    return NULL;
}

void destroy_hash_table(hash_table *table) {
    for(int i = 0; i < table->size; i++) {
        bucket *bkt = table->table[i];
        while(bkt != NULL) {
            bucket *existing = bkt;
            bkt = bkt->next;
            // should probably also free the values in the table, but let's just not
            free(existing);
        }
    }
    free(table);
}

static void resize_table(hash_table *table) {
    bucket **old_table = table->table;
    int old_size = table->size;
    table->size *= 2;
    table->table = calloc(table->size, sizeof(bucket *));
    for(int i = 0; i < old_size; i++) {
        bucket *next = NULL;
        for(bucket *bkt = old_table[i]; bkt != NULL; bkt = next) {
            int index = bkt->hash % table->size;
            next = bkt->next;
            bkt->next = table->table[index];
            table->table[index] = bkt;
        }
    }
    free(old_table);
}

static int hash(const char *key) {
    int hash = 1;
    // A pathetic hash function
    for(int i = 0; key[i] != '\0'; i++) {
        hash = (hash * key[i] + key[i]) ^ key[i];
    }
    return hash;
}
