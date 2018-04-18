package autojit
import org.objectweb.asm.Opcodes.ASM6
import org.objectweb.asm.{Label, MethodVisitor}

class InlineValidator(mv: MethodVisitor,
                      bannedOwner: String) extends MethodVisitor(ASM6, mv){
  var currentLine: Int = 0
  override def visitLineNumber(line: Int, start: Label): Unit = {
    currentLine = line
    super.visitLineNumber(line, start)
  }
  override def visitMethodInsn(opcode: Int, owner: String, name: String, descriptor: String, isInterface: Boolean): Unit = {
    if (owner == bannedOwner && isInterface) throw new Exception("Non-inlined method call slipped through: " + owner + " " + name + "" + descriptor + ":" + currentLine)
    else super.visitMethodInsn(opcode, owner, name, descriptor, isInterface)
  }
}
