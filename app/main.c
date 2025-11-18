#include "lex.h"
#include "parse.h"
#include "util.h"
#include <stdio.h>

int main(void) {
  List tokens = Lex((char *)"(s (expr testing) \"hello aaaaaa\")");

  Token *token = (Token*)list_grab(&tokens, 0);

  puts("Token Analysis:");
  
  for (int i = 0; token; i++) {
    printf("Token %d: %.*s\n", i, token->len, token->start);
    
    token = (Token*)list_grab(&tokens, i + 1);
  }
  
  List ast = parse(&tokens, 0);

  puts("\nParser Analysis:");
  
  print_ast(&ast);
  puts("\n");

  return 0;
}
