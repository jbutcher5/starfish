#include "util.h"

#include <stdlib.h>
#include <string.h>

#define ARRAY_INITIAL 32
#define ARRAY_AGGRESSION 2

String to_string(char *str) {
  int i = 0;
  for (; str[i]; i++);

  return (String){str, i};
}

uint8_t to_str(String string, char *dest, uint16_t limit) {
  int i = 0;
  for (; i < string.len; i++) {
    if (i >= limit)
      return 1;

    dest[i] = string.start[i];
  }

  if (i >= limit)
    return 1;

  dest[i] = 0;

  return 0;
}

List list_new(uint8_t type_size) {
  void *buffer = malloc(type_size * ARRAY_INITIAL);

  return (List){buffer, type_size, 0, ARRAY_INITIAL};
}

uint32_t list_push(List *list, void *data) {
  memcpy(list->buffer + list->size * list->type_size, data, list->type_size);
  
  list->size++;

  if (list->size >= list->alloc_size) {
    list->alloc_size *= ARRAY_AGGRESSION;

    list->buffer = realloc(list->buffer, list->type_size * list->alloc_size);
  }

  return list->size - 1;
}

void *list_get(List *list, uint32_t n) {
  if (n < list->size) {
    return list->buffer + n * list->type_size;
  }
  
  return 0;
}

void *list_pop(List *list) {
  void *result = list_get(list, list->size - 1);
  list->size -= (list->size > 0);

  return result;
}

List *list_slice(List *list, uint32_t start, uint32_t end) {
  List *result = malloc(sizeof(List));

  if (!result)
    return 0;
  
  *result = list_new(list->type_size);

  for (int i = 0; i < end - start; i++)
    list_push(result, list_get(list, start + i));

  return result;
}

void list_free(List *list) {
  free(list->buffer);

  list->size = 0;
  list->type_size = 0;
  list->buffer = 0;
  list->alloc_size = 0;
}

HashMap HM_Create(uint8_t length)
{
    return (HashMap) {
        .array = calloc(length, sizeof(HashMapBucket *)), .length = length, .__pred = 0x0
    };
}

uint8_t HM_Add(HashMap *map, uint32_t key, void *data)
{
    /* TODO: Remove this if statement and create a HM_Update function
     HM_Contains does not necessarily update the prediction.
     Maybe create internal function:
     
     struct HashMapBucket *HM_Find_Bucket(struct HashMap *map, uint32_t key);
   */
    if (HM_Contains(map, key))
    {
        map->__pred->data = data;
        return 1;
    }

    // Calculate hash using remainder
    uint8_t hash = key % map->length;

    // Allocate memory for new bucket on the heap
    HashMapBucket *new_bucket
        = (HashMapBucket *)calloc(1, sizeof(HashMapBucket));

    // Check calloc didn't fail
    if (!new_bucket) return 0;

    // Insert items into new bucket
    new_bucket->key  = key;
    new_bucket->data = data;
    new_bucket->next = 0x0;

    // Assign directly to array new bucket pointer
    if (!map->array[hash])
    {
        map->array[hash] = new_bucket;
        return 1;
    }

    // Get pointer to first bucket contained within array
    HashMapBucket *bucket = map->array[hash];

    // Otherwise traverse LL until last item
    while (bucket->next) bucket = bucket->next;

    // Assign new bucket pointer to next of last item in LL
    bucket->next = new_bucket;

    return 1;
}

uint8_t HM_Contains(HashMap *map, uint32_t key)
{
    // Calculate hash using remainder
    uint8_t hash = key % map->length;

    // If array does not contain anything return false
    if (!map->array[hash]) return 0;

    HashMapBucket *bucket = map->array[hash];

    // Traverse LL until Bucket is found with key
    for (; bucket->key != key; bucket = bucket->next)
        // If at the end of LL and not found return false
        if (!bucket->next) return 0;

    map->__pred = bucket;

    return 1;
}

void *HM_Get(HashMap *map, uint32_t key)
{
    // If a HM_Contains has just been ran before return pointer result
    if (map->__pred->key == key) return map->__pred->data;

    // Calculate hash using remainder
    uint8_t hash = key % map->length;

    // If array does not contain anything return false
    if (!map->array[hash]) return 0;

    HashMapBucket *bucket = map->array[hash];

    // Traverse LL until Bucket is found with key
    for (; bucket->key != key; bucket = bucket->next)
        // If at the end of LL and not found return false
        if (!bucket->next) return 0;

    return bucket->data;
}

uint8_t HM_Remove(HashMap *map, uint32_t key)
{
    // Calculate hash using remainder
    uint8_t hash = key % map->length;

    // If array does not contain anything
    if (!map->array[hash]) return 0;

    // If the key is the predicted key set the prediction to the next value in the LL
    if (map->__pred->key == key) map->__pred = map->__pred->next;

    HashMapBucket *bucket      = map->array[hash];
    HashMapBucket *last_bucket = 0;

    // Traverse LL until Bucket is found with key
    while (bucket->key != key)
    {
        // If at the end of LL and not found return error
        if (!bucket->next) return 0;

        // Update last_bucket and get next bucket
        last_bucket = bucket;
        bucket      = bucket->next;
    }

    // If it's in the middle of the LL assign last_bucket next to next bucket pointer
    if (last_bucket) last_bucket->next = bucket->next;
    // Otherwise assign directly to map->array
    else
        map->array[hash] = bucket->next;

    // Free bucket
    free(bucket);

    return 1;
}

void LL_Free(HashMapBucket *ptr)
{
    HashMapBucket *next = ptr->next;

    while (ptr->next)
    {
        free(ptr);
        ptr  = next;
        next = ptr->next;
    }

    free(ptr);
}

void HM_Free(HashMap *map)
{
    for (int i = map->length - 1; i; i--)
        if (map->array[i]) LL_Free(map->array[i]);

    free(map->array);
}
