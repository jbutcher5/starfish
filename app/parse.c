#include "parse.h"
#include "lex.h"
#include "util.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

List parse(List *tokens, uint16_t depth) {
  static uint8_t __paren_stack_init = 0;
  static List paren_stack;

  if (!__paren_stack_init) {
    paren_stack = list_new(sizeof(uint16_t));
    __paren_stack_init = 1;
  }
  
  List expr = list_new(sizeof(Cell));

  Token *t = (Token *)list_grab(tokens, 0);

  for (uint16_t i = 1; i < tokens->size; i++) {
    t = (Token *)list_grab(tokens, i);

    if (depth == paren_stack.size && t->type == LexerAtom) {
      ParserStringData *string = malloc(sizeof(ParserStringData));
      *string = (ParserStringData){.start = t->start, .len = t->len};
      Cell cell = {.type = ParserSymbol, .data = string};

      list_push(&expr, &cell);
    }

    if (depth == paren_stack.size && t->type == LexerString) {
      ParserStringData *string = malloc(sizeof(ParserStringData));
      *string = (ParserStringData){.start = t->start, .len = t->len};

      Cell cell = {.type = ParserString, .data = string};

      list_push(&expr, &cell);
    }

    if (t->type == LexerLeftParen)
      list_push(&paren_stack, &i);

    if (t->type == LexerRightParen) {
      uint16_t *last = (uint16_t*)list_pop(&paren_stack);

      if (last) {
	// Relies on copying buffer data. Bad.

        List *token_subset = list_slice(tokens, *last, i);
	List *parsed_subset = malloc(sizeof(Cell));
        *parsed_subset = parse(token_subset, paren_stack.size);
	list_free(token_subset);

	Cell cell = {.type = ParserSExpr, .data = (void*)parsed_subset};
	
	list_push(&expr, &cell);
      }

      else {
	return expr;
      }
    }
  }

  return expr;
}
