#pragma once
#include "util.h"
#include <stdint.h>
#include <stdbool.h>

typedef enum {
  LexerAtom,
  LexerString,
  LexerInt,
  LexerError,
  LexerLeftParen,
  LexerRightParen
} TokenType;

typedef struct {
  TokenType type;
  char *start;
  uint16_t len;
} Token;

List Lex(char *source);
