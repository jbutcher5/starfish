#include "lex.h"
#include "stdlib.h"

#define IS_ALPHA(x) (((x) >= 65 && (x) <= 90) || ((x) >= 97 && (x) <= 122))

Token Next(char *begin) {
  if (*begin == ' ' || *begin == '\n')
    return Next(begin + 1);

  if (*begin == '(')
    return (Token){.type = LeftParen, .start = begin, .len = 1};

  if (*begin == ')')
    return (Token){.type = RightParen, .start = begin, .len = 1};

  if (*begin == '"') {
    int i = 1;
    for (; begin[i] != '"'; i++) {}
    return (Token){.type = String, .start = begin, .len = i + 1};
  }

  if (IS_ALPHA(*begin)) {
    int i = 1;
    for (; IS_ALPHA(begin[i]); i++) {}
    return (Token){.type = Atom, .start = begin, .len = i + 1};
  }

  return (Token){.type = Error, .start = 0, .len = 0};
}

Token *Lex(char *source) {
  int32_t n = 0;
  int32_t max = 512;
  Token *buffer = calloc(max, sizeof(Token));

  if (!buffer)
    return 0;
  
  char *next_start = source;
  
  while (*next_start) {
    Token token = Next(next_start);
    next_start = token.start + token.len;
    buffer[n] = token;
    n++;

    if (n >= max - 1) {
      max *= 2;
      if (!realloc(buffer, sizeof(Token) * max)) {
	return 0;
      }
    }
  }

  return buffer;
}
