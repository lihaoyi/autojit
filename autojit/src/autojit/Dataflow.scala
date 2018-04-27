package autojit

import org.objectweb.asm.Opcodes.ASM4
import org.objectweb.asm.tree.analysis._
import org.objectweb.asm.{Opcodes, Type}
import org.objectweb.asm.tree.{AbstractInsnNode, LdcInsnNode, MethodInsnNode, VarInsnNode}

import collection.JavaConverters._
import scala.collection.mutable
case class Box(value: BasicValue,
               self: Option[AbstractInsnNode] = None,
               concrete: Option[(AbstractInsnNode, AbstractInsnNode)] = None,
               inlineable: Option[AbstractInsnNode] = None,
               fromSelf: Option[AbstractInsnNode] = None) extends Value {
  def getSize = value.getSize
}

class Dataflow(mergeLeft: Boolean,
               isTrivial: MethodInsnNode => Boolean,
               dataflowOwner: String) extends Interpreter[Box](ASM4){
  val internal = new BasicInterpreter()
  type AIN = AbstractInsnNode
  val metadata = mutable.Map.empty[AIN, Box]
  def newValue(tpe: org.objectweb.asm.Type) =
    if (tpe == null) Box(BasicValue.UNINITIALIZED_VALUE)
    else if (tpe.getSort == Type.VOID) null
    else Box(internal.newValue(tpe))


  def newOperation(insn: AIN) = {
    val res = Box(internal.newOperation(insn))
    metadata(insn) = res
    res
  }

  def copyOperation(insn: AIN, value: Box) = {
    val res = (insn, insn.getOpcode) match{
      case (vins: VarInsnNode, Opcodes.ALOAD) if vins.`var` == 0 =>
        Box(internal.copyOperation(insn, value.value), self = Some(insn), fromSelf = Some(insn))

      case _ => Box(
        internal.copyOperation(insn, value.value),
        fromSelf = value.fromSelf.map(_ => insn)
      )
    }
    metadata(insn) = res
    res
  }

  def unaryOperation(insn: AIN, value: Box) = {
    val res = Box(
      internal.unaryOperation(insn, value.value),
      fromSelf = value.fromSelf.map(_ => insn)
    )
    metadata(insn) = res
    res
  }
  def binaryOperation(insn: AIN, v1: Box, v2: Box) = {
    val res = Box(
      internal.binaryOperation(insn, v1.value, v2.value),
      fromSelf = v1.fromSelf.orElse(v2.fromSelf).map(_ => insn)
    )
    metadata(insn) = res
    res
  }
  def ternaryOperation(insn: AIN, v1: Box, v2: Box, v3: Box) = {
    val res = Box(
      internal.ternaryOperation(insn, v1.value, v2.value, v3.value),
      fromSelf = v1.fromSelf.orElse(v2.fromSelf).orElse(v3.fromSelf).map(_ => insn)
    )
    metadata(insn) = res
    res
  }
  def naryOperation(insn: AIN, vs: java.util.List[_ <: Box]) = (insn, insn.getOpcode) match {
    case (i: MethodInsnNode, Opcodes.INVOKEVIRTUAL) if i.name == "handle" =>
      val res = Box(
        internal.naryOperation(insn, vs.asScala.map(_.value).asJava),
        fromSelf = None
      )

      metadata(insn) = res
      res
    case (mins: MethodInsnNode, Opcodes.INVOKEVIRTUAL)
      if isTrivial(mins) && vs.get(0).self.isDefined =>
      val res = Box(
        internal.naryOperation(insn, vs.asScala.map(_.value).asJava),
        concrete = Some(vs.get(0).self.get -> insn)
      )

      metadata(insn) = res
      res

    case (mins: MethodInsnNode, Opcodes.INVOKEINTERFACE)
      if mins.owner == dataflowOwner && vs.get(0).concrete.nonEmpty =>

      val res = Box(
        internal.naryOperation(insn, vs.asScala.map(_.value).asJava),
        inlineable = Some(vs.get(0).concrete.get._2)
      )
      metadata(insn) = res
      res

    case _ =>

      val res = Box(
        internal.naryOperation(insn, vs.asScala.map(_.value).asJava),
        fromSelf = vs.asScala.map(_.fromSelf).foldLeft(Option.empty[AbstractInsnNode])(_.orElse(_))
      )

      metadata(insn) = res
      res
  }
  def returnOperation(insn: AIN, value: Box, expected: Box) = {}
  def merge(v1: Box, v2: Box) = (if (mergeLeft) v1 else v2).copy(fromSelf = None)

}
