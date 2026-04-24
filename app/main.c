#include "ir_gen.h"
#include "lex.h"
#include "parse.h"
#include "util.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
  char *tmp = "test2.star";
  FILE *file = fopen(tmp, "r");
  //FILE *file = fopen(argv[1], "r");
  List characters = list_new(sizeof(char));

  char ch;
  
  while ((ch = fgetc(file)) != EOF) {
    list_push(&characters, &ch);
  }

  fclose(file);
  
  ch = 0;
  list_push(&characters, &ch);
  
  List tokenlist = Lex(characters.buffer);

  Token *token = (Token*)list_grab(&tokenlist, 0);

  puts("Token Analysis:");
  
  for (int i = 0; token; i++) {
    printf("Token %d: %.*s %d\n", i, token->len, token->start, token->type);
    
    token = (Token*)list_grab(&tokenlist, i + 1);
  }

  List *ast = (List*)malloc(sizeof(List));
  *ast = parse(&tokenlist);

  list_free(&tokenlist);
  
  puts("\nParser Analysis:");

  print_ast(ast);

  puts("");

  List ir = ast_to_ir(ast);
  

  char name[128];

  IR *x = ir.buffer;

  printf("%d\n", ast->size);

  if (x && x->tag == IRFunc) {
    to_str(x->data.IRFunc.identifier, name, 128);
    printf("%s", name);
  } else if (x && x->tag == IRVar) {
    puts("Arrrrr");
  }

  list_free(&characters);
  free_ast(ast);


  return 0;
}
