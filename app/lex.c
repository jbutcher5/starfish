#include "lex.h"
#include "util.h"

#define IS_ALPHA(x) (((x) >= 65 && (x) <= 90) || ((x) >= 97 && (x) <= 122) || ((x) >= 42 && (x) <= 46))
#define IS_NUM(x) ((x) >= 48 && (x) <= 57)

Token Next(char *begin) {
  if (*begin == ' ' || *begin == '\n')
    return Next(begin + 1);

  if (*begin == '(')
    return (Token){.type = LexerLeftParen, .start = begin, .len = 1};

  if (*begin == ')')
    return (Token){.type = LexerRightParen, .start = begin, .len = 1};

  if (*begin == '"') {
    uint16_t i = 1;
    while (begin[i] != '"') i++;
    return (Token){.type = LexerString, .start = begin, .len = i + 1};
  }

  if (IS_NUM(*begin)) {
    uint16_t i = 1;
    while (IS_NUM(begin[i])) i++;
    return (Token){.type = LexerInt, .start = begin, .len = i};
  }
  
  if (IS_ALPHA(*begin) || IS_NUM(*begin)) {
    uint16_t i = 1;
    while (IS_ALPHA(begin[i])) i++;
    return (Token){.type = LexerAtom, .start = begin, .len = i};
  }

  return (Token){.type = LexerError, .start = begin, .len = 1};
}

List Lex(char *source) {
  List list = list_new(sizeof(Token));
  
  char *next_start = source;
  
  while (*next_start) {
    Token token = Next(next_start);
    next_start = token.start + token.len;
    
    list_push(&list, &token);
  }

  return list;
}
