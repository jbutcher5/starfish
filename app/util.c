#include "util.h"

#include <stdlib.h>
#include <string.h>

#define ARRAY_INITIAL 32
#define ARRAY_AGGRESSION 2

List list_new(uint8_t type_size) {
  void *buffer = malloc(type_size * ARRAY_INITIAL);

  return (List){buffer, type_size, 0, ARRAY_INITIAL};
}

uint8_t list_push(List *list, void *data) {
  memcpy(list->buffer + list->size * list->type_size, data, list->type_size);
  
  list->size++;

  if (list->size >= list->alloc_size) {
    list->alloc_size *= ARRAY_AGGRESSION;

    list->buffer = realloc(list->buffer, list->type_size * list->alloc_size);

    return list->buffer ? 1 : 0;
  }
  
  return 1;
}

void *list_grab(List *list, uint32_t n) {
  if (n < list->size) {
    return list->buffer + n * list->type_size;
  }
  
  return 0;
}

void *list_pop(List *list) {
  void *result = list_grab(list, list->size - 1);
  list->size -= (list->size > 0);

  return result;
}
