#pragma once
#include <stdint.h>

typedef struct {
  char *start;
  uint16_t len;
} String;

typedef struct {
  void *buffer;
  uint8_t type_size;
  uint32_t size;
  uint32_t alloc_size;
} List;

typedef struct HashMapBucket {
    uint32_t              key;
    void                 *data;
    struct HashMapBucket *next;
} HashMapBucket;

typedef struct
{
    struct HashMapBucket **array;
    uint8_t                length;
    struct HashMapBucket  *__pred;
} HashMap;

HashMap HM_Create(uint8_t length);
uint8_t HM_Add(HashMap *map, uint32_t key, void *data);
uint8_t HM_Contains(HashMap *map, uint32_t key);
void *HM_Get(HashMap *map, uint32_t key);
uint8_t HM_Remove(HashMap *map, uint32_t key);
void HM_Free(HashMap *map);

String to_string(char *str);
uint8_t to_str(String string, char *dest, uint16_t limit);
List list_new(uint8_t type_size);
uint8_t list_push(List *list, void *data);
void *list_grab(List *list, uint32_t n);
void *list_pop(List *list);
List *list_slice(List *list, uint32_t start, uint32_t end);
void list_free(List *list);
