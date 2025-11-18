#pragma once
#include <stdint.h>

typedef struct {
  void *buffer;
  uint8_t type_size;
  uint32_t size;
  uint32_t alloc_size;
} List;

List list_new(uint8_t type_size);
uint8_t list_push(List *list, void *data);
void *list_grab(List *list, uint32_t n);
void *list_pop(List *list);

