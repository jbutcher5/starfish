#include "lex.h"
#include "stdlib.h"

int main(void) {
  Token *program = Lex("(s expr testing \"hello aaaaaa\")");

  free(program);


  return 0;
}
