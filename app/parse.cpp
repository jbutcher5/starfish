#include "parse.hpp"
#include "lex.hpp"
#include "util.hpp"
#include <cstdlib>

S_Expr parse(Array<Token> tokens) {
  S_Expr expr = S_Expr();

  Maybe<Token> token = tokens.get(0);

  for (int i = 1; token.is_some(); i++) {
    // TODO: Iterate to tokens push them back in expr
    // if the token is a '(' start a new thread and join the result to expr. skip to equiv. ')' and continue from there 
  }

  return expr;
}
