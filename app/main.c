#include "lex.h"
#include "parse.h"
#include "util.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
  FILE *file = fopen(argv[1], "r");
  List characters = list_new(sizeof(char));

  char ch;
  
  while ((ch = fgetc(file)) != EOF) {
    list_push(&characters, &ch);
  }

  fclose(file);
  
  ch = 0;
  list_push(&characters, &ch);
  
  List tokens = Lex(characters.buffer);

  Token *token = (Token*)list_grab(&tokens, 0);

  puts("Token Analysis:");
  
  for (int i = 0; token; i++) {
    printf("Token %d: %.*s %d\n", i, token->len, token->start, token->type);
    
    token = (Token*)list_grab(&tokens, i + 1);
  }

  List *ast = (List*)malloc(sizeof(List));
  *ast = parse(&tokens);

  list_free(&tokens);
  
  puts("\nParser Analysis:");

  print_ast(ast);

  puts("");

  list_free(&tokens);
  list_free(&characters);
  free_ast(ast);

  return 0;
}
