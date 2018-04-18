package autojit

import org.objectweb.asm.Opcodes.ASM4
import org.objectweb.asm.tree.analysis._
import org.objectweb.asm.{Opcodes, Type}
import org.objectweb.asm.tree.{AbstractInsnNode, LdcInsnNode, MethodInsnNode, VarInsnNode}

import collection.JavaConverters._
case class Box(value: BasicValue,
               self: Boolean = false,
               concrete: Option[AbstractInsnNode] = None,
               inlineable: Option[AbstractInsnNode] = None) extends Value {
  def getSize = value.getSize
}

class Dataflow(mergeLeft: Boolean,
               isTrivial: MethodInsnNode => Boolean,
               dataflowOwner: String) extends Interpreter[Box](ASM4){
  val internal = new BasicInterpreter()
  type AIN = AbstractInsnNode

  def newValue(tpe: org.objectweb.asm.Type) =
    if (tpe == null) new Box(BasicValue.UNINITIALIZED_VALUE)
    else if (tpe.getSort == Type.VOID) null
    else new Box(internal.newValue(tpe))


  def newOperation(insn: AIN) = new Box(internal.newOperation(insn))

  def copyOperation(insn: AIN, value: Box) = (insn, insn.getOpcode) match{
    case (vins: VarInsnNode, Opcodes.ALOAD) if vins.`var` == 0 =>
      new Box(internal.copyOperation(insn, value.value), self = true)

    case (i, o) => new Box(internal.copyOperation(insn, value.value))
  }

  def unaryOperation(insn: AIN, value: Box) = new Box(internal.unaryOperation(insn, value.value))
  def binaryOperation(insn: AIN, v1: Box, v2: Box) = new Box(internal.binaryOperation(insn, v1.value, v2.value))
  def ternaryOperation(insn: AIN, v1: Box, v2: Box, v3: Box) = new Box(internal.ternaryOperation(insn, v1.value, v2.value, v3.value))
  def naryOperation(insn: AIN, vs: java.util.List[_ <: Box]) = (insn, insn.getOpcode) match {
    case (mins: MethodInsnNode, Opcodes.INVOKEVIRTUAL)
      if isTrivial(mins) && vs.get(0).self =>
      new Box(
        internal.naryOperation(insn, vs.asScala.map(_.value).asJava),
        concrete = Some(insn)
      )
    case (mins: MethodInsnNode, Opcodes.INVOKEINTERFACE)
      if mins.owner == dataflowOwner && vs.get(0).concrete.nonEmpty =>
      new Box(
        internal.naryOperation(insn, vs.asScala.map(_.value).asJava),
        inlineable = Some(vs.get(0).concrete.get)
      )
    case _ => new Box(internal.naryOperation(insn, vs.asScala.map(_.value).asJava))
  }
  def returnOperation(insn: AIN, value: Box, expected: Box) = ()
  def merge(v1: Box, v2: Box) = if (mergeLeft) v1 else v2
}
