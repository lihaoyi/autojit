package autojit
import org.objectweb.asm.Opcodes.ASM6
import org.objectweb.asm.{Label, MethodVisitor, Opcodes}

class InlineValidator(mv: MethodVisitor,
                      bannedOwner: String) extends MethodVisitor(ASM6, mv){
  var currentLine: Int = 0
  override def visitLineNumber(line: Int, start: Label): Unit = {
    currentLine = line
    super.visitLineNumber(line, start)
  }
//
//  override def visitVarInsn(opcode: Int, `var`: Int): Unit = {
//    if (opcode == Opcodes.ALOAD && `var` == 0)throw new Exception(
//      "Non-inlined self reference slipped through: " + currentLine
//    )
//    else super.visitVarInsn(opcode, `var`)
//  }
  override def visitMethodInsn(opcode: Int,
                               owner: String,
                               name: String,
                               descriptor: String,
                               isInterface: Boolean): Unit = {
    if (owner == bannedOwner && isInterface) {
      throw new Exception(
        "Non-inlined method call slipped through: " +
        owner + " " + name + "" + descriptor + ":" + currentLine
      )
    } else {
      super.visitMethodInsn(opcode, owner, name, descriptor, isInterface)
    }
  }
}
