#include "ir_gen.h"
#include "parse.h"

Type *create_type(String str) {
  if (str.len < 3) {
    puts("Invalid type");
    exit(2);
    return 0;
  }

  Type *result = (Type *)malloc(sizeof(Type));
  
  if (str.start[0] == '*') {
    result->tag = TYPETAG_POINTER;
    result->detail.deref = create_type((String){str.start + 1, str.len - 1});
    return result;
  }

  else if (strcmp_n(str.start, "Int", 3)) {
    result->tag = TYPETAG_PRIMITIVE;
    result->detail.primitive = PRIMITIVE_CHAR;

    return result;
  }

  else if (strcmp_n(str.start, "Char", 4)) {
    result->tag = TYPETAG_PRIMITIVE;
    result->detail.primitive = PRIMITIVE_INTEGRAL;

    return result;
  }

  free(result);
  return 0;
}

uint8_t type_size(Type t) {
  if (t.tag == TYPETAG_POINTER)
    return 1;
  if (t.tag == TYPETAG_PRIMITIVE) {
    if (t.detail.primitive == PRIMITIVE_CHAR)
      return 1;
    else if (t.detail.primitive == PRIMITIVE_INTEGRAL)
      return 4;
  }

  return 0;
}

uint8_t strcmp_n(const char *s1, const char *s2, uint16_t n) {
  for (uint16_t i = 0; i < n; i++)
    if (!s1[i] || !s2[i] || s1[i] != s2[i])
      return 0;

  return 1;
}

AST ast_define(List *l) {
  if (l->size != 4)
    exit(2);

  AST node;
  node.tag = ASTVar;

  Cell *t_s = list_grab(l, 1);
  Cell *ident_s = list_grab(l, 2);
  Cell *ast_var = list_grab(l, 3);

  if (t_s->tag != ParserSymbol || ident_s->tag != ParserSymbol)
    exit(2);

  node.data.ASTVar.identifier = ident_s->data.ParserString;
  node.data.ASTVar.type = create_type(t_s->data.ParserString);
  node.data.ASTVar.node = cell_to_ast(ast_var);

  return node;
}

AST (*sexpr_f[])(List *) = {ast_define};
const char *sexpr_kw[] = {"define"};

AST *get_match(List *l) {
  static AST result;

  if (!l->size) {
    result = (AST){ASTIntegral, .data = 0};
    return &result;
  }
    
  Cell *first_cell = list_grab(l, 0);
  
  if (first_cell->tag != ParserSymbol) {
    exit(2);
  }
  
  String first_word = first_cell->data.ParserString;

  for (int i = 0; i < sizeof(sexpr_kw) / sizeof(char *); i++) {
    if (strcmp_n(first_word.start, sexpr_kw[i], first_word.len)) {
      result = sexpr_f[i](l);
      return &result;
    }
  }

  return 0;
}

AST cell_to_ast(Cell *cell) {
  if (cell->tag == ParserSExpr) {
    AST *result = get_match(cell->data.ParserSExpr);

    if (!result) {
      exit(2);
    }
    
    return *result;
  }
}

List ast_to_ir(List *ast) {
  return list_new(sizeof(AST));
}
