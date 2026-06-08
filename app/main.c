#include "ir_gen.h"
#include "lex.h"
#include "parse.h"
#include "system_v.h"
#include "util.h"
#include <stdio.h>
#include <stdlib.h>

IRAlloc ir_alloc;
SysVAlloc sysv_alloc;

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

  Token *token = (Token*)list_get(&tokenlist, 0);

  /*
  puts("Token Analysis:");
  
  for (int i = 0; token; i++) {
    printf("Token %d: %.*s %d\n", i, token->len, token->start, token->type);
    
    token = (Token*)list_get(&tokenlist, i + 1);
  }
  */

  List *ast = (List*)malloc(sizeof(List));
  *ast = parse(&tokenlist);

  list_free(&tokenlist);
  
  ir_alloc.ir_nodes = list_new(sizeof(IR));
  ir_alloc.types = list_new(sizeof(Type));
  ir_alloc.lists = list_new(sizeof(List));

  List ir = ast_to_ir(ast);

  sysv_alloc.offets = list_new(sizeof(SizedOffset));

  List sysv = ir_to_sysv(&ir);
  output_sysv(sysv);

  list_free(&sysv_alloc.offets);
  list_free(&ir_alloc.ir_nodes);
  list_free(&ir_alloc.types);

  for (int i = 0; i < ir_alloc.lists.size; i++) {
    List *list = list_get(&ir_alloc.lists, i);

    if (list) {
      list_free(list);
    }
  }

  list_free(&ir_alloc.lists);
  list_free(&ir);
  list_free(&sysv);

  list_free(&characters);
  free_ast(ast);

  return 0;
}
