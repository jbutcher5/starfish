#include "system_v.h"
#include "ir_gen.h"
#include "util.h"

void append_sysv(Enviroment *env, IR ir) {
  if (ir.tag == IRVar) {
    SysV var;
    var.tag = SysVVar;
    var.data.SysVVar.identifier = ir.data.IRVar.identifier;
    var.data.SysVVar.size = type_size(*ir.data.IRVar.type);

    env->current_offsets += var.data.SysVVar.size;
    var.data.SysVVar.offset = env->current_offsets;
    
    list_push(&env->sysv_code, (void*)&var);
  }
}
