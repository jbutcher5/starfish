#include "parse.h"
#include "lex.h"
#include "util.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>

void atoi_n(uint64_t *result, char *str, uint16_t length) {
  *result = 0;

  for (int i = 0; i < length; i++)
    *result += (str[i] - 48) * (i + 1);
}

List *get_insertion_list(List *ast, List *depth) {
  List *insertion_list = ast;

  for (uint32_t j = 0; j < depth->size; j++) {
    uint16_t *depth_i = list_grab(depth, j);
    Cell *sexpr = list_grab(insertion_list, *depth_i);
	
    if (sexpr->tag != ParserSExpr) {
      puts("Expected to index an sexpr is not");
      exit(1);
    }
      
    insertion_list = sexpr->data.ParserSExpr;
  }

  return insertion_list;

}

List parse(List *tokens) {
  List ast = list_new(sizeof(Cell));
  List depth = list_new(sizeof(uint32_t));

  List *insertion_list = &ast;
  Cell cell;

  for (uint32_t i = 0; i < tokens->size; i++) {
    
    Token *t = (Token *)list_grab(tokens, i);

    if (t->type == LexerAtom) {
      ParserStringData *string = malloc(sizeof(ParserStringData));
      *string = (ParserStringData){.start = t->start, .len = t->len};

      cell = (Cell){.tag = ParserSymbol, {.ParserString = string}};
      list_push(insertion_list, &cell);
    }

    else if (t->type == LexerString) {
      ParserStringData *string = malloc(sizeof(ParserStringData));
      *string = (ParserStringData){.start = t->start, .len = t->len};

      cell = (Cell){.tag = ParserString, {.ParserString = string}};
      list_push(insertion_list, &cell);
    }

    else if (t->type == LexerInt) {
      uint64_t i;

      atoi_n(&i, t->start, t->len);

      cell = (Cell){.tag = ParserInt, {.ParserInt = i}};
      list_push(insertion_list, &cell);
    }

    else if (t->type == LexerLeftParen) {
      List *list = malloc(sizeof(List));
      *list = list_new(sizeof(Cell));

      cell = (Cell){.tag = ParserSExpr, {.ParserSExpr=list}};
      list_push(insertion_list, &cell);

      uint32_t location = insertion_list->size - 1;
      list_push(&depth, &location);

      insertion_list = get_insertion_list(&ast, &depth);
    }

    else if (t->type == LexerRightParen) {
      list_pop(&depth);
      insertion_list = get_insertion_list(&ast, &depth);
    }
  }

  return ast;
}


void free_cell(Cell *cell) {
  //free(cell->data);

  cell->tag = ParserVoid;
}

void print_ast(List *ast) {
  Cell *cell = list_grab(ast, 0);

  printf("( ");
  
  for (uint16_t i = 1; cell; i++) {

    if (cell->tag == ParserString || cell->tag == ParserSymbol) {
      ParserStringData *string = cell->data.ParserString;

      printf("%.*s ", string->len, string->start);
    }

    if (cell->tag == ParserInt)
      printf("%" PRIu64 " ", cell->data.ParserInt);

    if (cell->tag == ParserSExpr) {
      print_ast(cell->data.ParserSExpr);
    }
    
    cell = list_grab(ast, i);
  }

  printf(") ");
}
