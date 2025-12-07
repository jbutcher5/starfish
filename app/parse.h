#pragma once
#include <stdint.h>
#include "util.h"

typedef struct {
  char *start;
  uint16_t len;
} ParserStringData;

typedef struct {
  enum { ParserString, ParserSymbol, ParserInt, ParserSExpr, ParserVoid } tag;

  union {
    ParserStringData *ParserString;
    ParserStringData *ParserSymbol;
    uint64_t ParserInt;
    List *ParserSExpr;
  } data;
} Cell;


List parse(List *tokens);
void free_cell(Cell *cell);
void print_ast(List *ast);
