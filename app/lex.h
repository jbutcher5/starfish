#pragma once
#include <stdint.h>
#include <stdbool.h>

typedef enum {
  Atom,
  String,
  Error,
  LeftParen,
  RightParen
} TokenType;

typedef struct {
  TokenType type;
  char *start;
  uint16_t len;
} Token;

Token *Lex(char *source);
