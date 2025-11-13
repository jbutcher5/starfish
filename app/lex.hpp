#pragma once
#include "util.hpp"
#include <stdint.h>
#include <stdbool.h>

typedef enum {
  LexerAtom,
  LexerString,
  LexerError,
  LexerLeftParen,
  LexerRightParen
} TokenType;

typedef struct {
  TokenType type;
  char *start;
  uint16_t len;
} Token;

Array<Token> Lex(char *source);
