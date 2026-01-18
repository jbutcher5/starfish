#include "system_v.h"
#include "ir_gen.h"
#include "util.h"

uint32_t djb2_hash(String str) {
  uint32_t hash = 5381;

  for (uint16_t i = 0; i < str.len; i++)
    hash = 33 * hash + str.start[i];

  return hash;
}

void append_sysv(Environment *env, IR ir) {
  if (ir.tag == IRVar) {
    SysV var;
    var.tag = SysVVar;
    var.data.SysVVar.identifier = ir.data.IRVar.identifier;
    var.data.SysVVar.size = type_size(*ir.data.IRVar.type);

    env->current_offsets += var.data.SysVVar.size;
    var.data.SysVVar.offset = env->current_offsets;

    uint16_t *offset = malloc(sizeof(uint16_t));
    *offset = env->current_offsets;

    HM_Add(&env->offsets,
           djb2_hash(var.data.SysVVar.identifier), (void*)offset);
    
    list_push(&env->sysv_code, (void*)&var);
  }
}

List ir_to_sysv(List *ir) {
  Environment env = {
    HM_Create(64),
    0,
    list_new(sizeof(SysV))
  };
  
  for (uint32_t i = 0; i < ir->size; i++) {
    append_sysv(&env, *(IR *)list_grab(ir, i));
  }

  return env.sysv_code;
}
