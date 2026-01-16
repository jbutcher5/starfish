#include "ir_gen.h"
#include "parse.h"
#include "util.h"

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

IR ir_define(List *l) {
  if (l->size != 4)
    exit(2);

  IR node;
  node.tag = IRVar;

  Cell *t_s = list_grab(l, 1);
  Cell *ident_s = list_grab(l, 2);
  Cell *ast_var = list_grab(l, 3);

  if (t_s->tag != ParserSymbol || ident_s->tag != ParserSymbol)
    exit(2);

  node.data.IRVar.identifier = ident_s->data.ParserString;
  node.data.IRVar.type = create_type(t_s->data.ParserString);
  node.data.IRVar.node = cell_to_ir(ast_var);

  return node;
}

IR (*sexpr_f[])(List *) = {ir_define};
const char *sexpr_kw[] = {"define"};

IR *get_match(List *l) {
  static IR result;

  if (!l->size) {
    return 0;
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
IR* cell_to_ir(Cell *cell) {
  IR node;
  
  if (cell->tag == ParserSExpr) {
    IR *s_expr_match = get_match(cell->data.ParserSExpr);

    if (!s_expr_match)
      puts("SExpr has no match");
    
    node = *s_expr_match;
  }

  else if (cell->tag == ParserInt) {
    node.tag = IRInt;
    node.data.IRInt = cell->data.ParserInt;
  }

  else if (cell->tag == ParserString) {
    node.tag = IRString;
    node.data.IRString = cell->data.ParserString;
  }

  else if (cell->tag == ParserSymbol) {
    node.tag = IRVarRef;
    node.data.IRVarRef = cell->data.ParserString;
  }

  else if (cell->tag == ParserVoid) {
    exit(3);
  }

  IR *result = (IR *)malloc(sizeof(IR));

  *result = node;
  
  return result;
}

List ast_to_ir(List *ast) {
  List result = list_new(sizeof(IR));

  for (int i = 0; i < ast->size; i++) {
    list_push(&result, cell_to_ir(list_grab(ast, i)));
  }

  return result;
}
