#include "lex.hpp"
#include "stdlib.h"
#include "util.hpp"
#include <stdio.h>

int main(void) {
  Array<Token> program = Lex((char *)"(s expr testing \"hello aaaaaa\")");

  Maybe<Token> token = program.get(0);

  for (int i = 0; token.is_some(); i++) {
    token = program.get(i);
    printf("%.*s\n", token.unwrap().len, token.unwrap().start);
  }

  return 0;
}
