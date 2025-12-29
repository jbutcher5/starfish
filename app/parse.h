#pragma once
#include <stdint.h>
#include "util.h"

typedef struct {
  enum { ParserString, ParserSymbol, ParserInt, ParserSExpr, ParserVoid } tag;

  union {
    String ParserString;
    uint64_t ParserInt;
    List *ParserSExpr;
  } data;
} Cell;

List parse(List *tokens);
void free_ast(List *ast);
void print_ast(List *ast);
