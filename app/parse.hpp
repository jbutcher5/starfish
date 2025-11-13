#pragma once
#include <stdint.h>
#include "lex.hpp"
#include "util.hpp"

typedef enum {
  ParserString,
  ParserSymbol
} ParserType;

typedef struct {
  void *data;
  ParserType type;
} Cell;

typedef Array<Cell> S_Expr;

S_Expr parse(Array<Token> tokens);
