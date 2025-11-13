#include "lex.hpp"
#include "util.hpp"
#include "stdlib.h"

#define IS_ALPHA(x) (((x) >= 65 && (x) <= 90) || ((x) >= 97 && (x) <= 122))

Token Next(char *begin) {
  if (*begin == ' ' || *begin == '\n')
    return Next(begin + 1);

  if (*begin == '(')
    return (Token){.type = LexerLeftParen, .start = begin, .len = 1};

  if (*begin == ')')
    return (Token){.type = LexerRightParen, .start = begin, .len = 1};

  if (*begin == '"') {
    uint16_t i = 1;
    for (; begin[i] != '"'; i++) {}
    return (Token){.type = LexerString, .start = begin, .len = static_cast<uint16_t>(i + 1)};
  }

  if (IS_ALPHA(*begin)) {
    uint16_t i = 1;
    for (; IS_ALPHA(begin[i]); i++) {}
    return (Token){.type = LexerAtom, .start = begin, .len = static_cast<uint16_t>(i + 1)};
  }

  return (Token){.type = LexerError, .start = begin, .len = 1};
}

Array<Token> Lex(char *source) {
  int32_t n = 0;
  int32_t max = 512;
  Array<Token> array = Array<Token>();
  
  char *next_start = source;
  
  while (*next_start) {
    Token token = Next(next_start);
    next_start = token.start + token.len;
    
    array.push(token);
  }

  return array;
}
