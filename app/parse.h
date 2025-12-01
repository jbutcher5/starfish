#pragma once
#include <stdint.h>
#include "util.h"

typedef enum {
  ParserString,
  ParserSymbol,
  ParserInt,
  ParserSExpr,
  ParserVoid
} ParserType;

typedef struct {
  ParserType type;
  void *data;
} Cell;

typedef struct {
  char *start;
  uint16_t len;
} ParserStringData;

List parse(List *tokens);
void free_cell(Cell *cell);
void print_ast(List *ast);
