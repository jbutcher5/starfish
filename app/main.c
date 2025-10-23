#include "lex.h"
#include "stdlib.h"
#include <stdio.h>

int main(void) {
  Token *program = Lex("(s expr testing \"hello aaaaaa\")");

  for (int i = 0; i < 7; i++)
    printf("%.*s\n", program[i].len, program[i].start);
  
  free(program);


  return 0;
}
