#include "ir_gen.h"
#include "parse.h"
#include "util.h"
#include <asm-generic/errno.h>
#include <stdlib.h>

extern IRAlloc ir_alloc;

Type create_type(String str) {
  if (str.len < 3) {
    exit(2);
  }

  Type result;
  
  if (str.start[0] == '*') {
    result.tag = TYPETAG_POINTER;
    
    Type deref = create_type((String){str.start + 1, str.len - 1});
    result.detail.deref = list_push(&ir_alloc.types, (void *)&deref);
  }

  else if (strcmp_n(str.start, "Int", 3)) {
    result.tag = TYPETAG_PRIMITIVE;
    result.detail.primitive = PRIMITIVE_INTEGRAL;
  }

  else if (strcmp_n(str.start, "Char", 4)) {
    result.tag = TYPETAG_PRIMITIVE;
    result.detail.primitive = PRIMITIVE_CHAR;
  }

  else {
    exit(2);
  }

  return result;
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

  Cell *t_s = list_get(l, 1);
  Cell *ident_s = list_get(l, 2);
  Cell *ast_var = list_get(l, 3);

  if (t_s->tag != ParserSymbol || ident_s->tag != ParserSymbol)
    exit(2);

  IR node;
  node.tag = IRVar;

  Type type = create_type(t_s->data.ParserString);
  uint32_t type_handle = list_push(&ir_alloc.types, (void*)&type);
  
  IR *rhs = cell_to_ir(ast_var);
  uint32_t rhs_handle = list_push(&ir_alloc.ir_nodes, (void*)rhs);

  node.data.IRVar.identifier = ident_s->data.ParserString;
  node.data.IRVar.type = type_handle;
  node.data.IRVar.node = rhs_handle;

  return node;
}

IR ir_defun(List *l) {
  IR node;
  node.tag = IRFunc;

  Cell *name = list_get(l, 1);
  Cell *ret_type = list_get(l, 2);
  Cell *args = list_get(l, 3);

  if (name->tag != ParserSymbol || ret_type->tag != ParserSymbol || args->tag != ParserSExpr)
    exit(2);

  List *body_ast = list_slice(l, 4, l->size);
  List body_ir = ast_to_ir(body_ast);

  if (body_ast) {
    list_free(body_ast);
    free(body_ast);
  }

  uint32_t body_handle = list_push(&ir_alloc.lists, (void*)&body_ir);
  
  node.data.IRFunc.body = body_handle;

  node.data.IRFunc.identifier = name->data.ParserString;

  Type t = create_type(ret_type->data.ParserString);
  uint32_t ret_handle = list_push(&ir_alloc.types, (void*)&t);

  node.data.IRFunc.ret_type = ret_handle;

  List param_types_proto = list_new(sizeof(TypedIdent));
  uint32_t handle = list_push(&ir_alloc.lists, (void*)&param_types_proto);

  List *param_types = list_get(&ir_alloc.lists, handle);

  for (int i = 0; i < args->data.ParserSExpr->size; i++) {
    Cell *type_pair = list_get(args->data.ParserSExpr, i);
    
    if (type_pair->tag != ParserSExpr || type_pair->data.ParserSExpr->size != 2)
      exit(2);

    Cell *ident = list_get(type_pair->data.ParserSExpr, 0);
    Cell *type = list_get(type_pair->data.ParserSExpr, 1);

    if (ident->tag != ParserSymbol || type->tag != ParserSymbol)
      exit(2);

    Type t = create_type(type->data.ParserString);
    uint32_t t_handle = list_push(&ir_alloc.types, (void *)&t);

    TypedIdent param = {
      .ident = ident->data.ParserString,
      .type = t_handle
    };
  
    list_push(param_types, &param);
  }

  node.data.IRFunc.param_types = handle;

  return node;
}

IR ir_if(List *l) {
  IR node;
  node.tag = IRIf;

  Cell *condition = list_get(l, 1);
  Cell *a = list_get(l, 2);
  Cell *b = list_get(l, 3);

  uint32_t handle;
  IR *ir;

  ir = cell_to_ir(condition);
  handle = list_push(&ir_alloc.ir_nodes, (void*)ir);
  node.data.IRIf.condition = handle;

  ir = cell_to_ir(a);
  handle = list_push(&ir_alloc.ir_nodes, (void*)ir);
  node.data.IRIf.a = handle;

  ir = cell_to_ir(b);
  handle = list_push(&ir_alloc.ir_nodes, (void*)ir);
  node.data.IRIf.b = handle;

  return node;
}

IR (*sexpr_f[])(List *) = {ir_define, ir_defun, ir_if};
const char *sexpr_kw[] = {"define", "fn", "if"};

IR *get_match(List *l) {
  static IR result;

  if (!l->size) {
    return 0;
  }
    
  Cell *first_cell = list_get(l, 0);
  
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

IR *cell_to_ir(Cell *cell) {
  static IR node;
  
  if (cell->tag == ParserSExpr) {
    IR *s_expr_match = get_match(cell->data.ParserSExpr);

    //if (!s_expr_match) {
      // TODO: Instead verify that function actually exists but instead just call it

      
    //}

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

  return &node;
}

List ast_to_ir(List *ast) {
  List result = list_new(sizeof(IR));

  for (int i = 0; i < ast->size; i++) {
    IR *ir = cell_to_ir(list_get(ast, i));
    list_push(&result, ir);
  }

  return result;
}
