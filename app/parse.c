#include "parse.h"
#include "lex.h"
#include "util.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

List parse(List *tokens) {
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

    if (t->type == LexerAtom) {
      ParserStringData *string = malloc(sizeof(ParserStringData));
      *string = (ParserStringData){.start = t->start, .len = t->len};
      Cell cell = {.type = ParserSymbol, .data = string};

      list_push(&expr, &cell);
    }

    if (t->type == LexerString) {
      ParserStringData *string = malloc(sizeof(ParserStringData));
      *string = (ParserStringData){.start = t->start, .len = t->len};

      Cell cell = {.type = ParserString, .data = string};
      
      list_push(&expr, &cell);
    }

    if (t->type == LexerLeftParen)
      list_push(&paren_stack, &i);

    if (t->type == LexerRightParen) {
      uint16_t *last = (uint16_t*)list_pop(&paren_stack);

      printf("AAhhhh\n");

      if (last) {
	
	printf("Popped a ')' where '(' starts at %d", *last);
	//S_Expr *sexpr = parse(tokens.clone_slice(last.unwrap(), i).unwrap());

        //list_push(
        //    (Cell){.type = ParserSExpr, .data = sexpr});
      }

      else {
	return expr;
      }
    }
  }

  return expr;
}
