#include "lex.h"
#include "parse.h"
#include "util.h"
#include <stdio.h>

int main(void) {
  List tokens = Lex((char *)"(s (expr testing) \"hello aaaaaa\")");

  Token *token = (Token*)list_grab(&tokens, 0);

  for (int i = 0; token; i++) {
    printf("%.*s\n", token->len, token->start);
    
    token = (Token*)list_grab(&tokens, i + 1);
  }

  List ast = parse(&tokens, 0);
  return 0;
}
