package autojit

import org.objectweb.asm.Opcodes.ASM6
import org.objectweb.asm._
import org.objectweb.asm.tree._

import collection.JavaConverters._
import scala.collection.mutable
sealed trait Expr{ def eval(): Int }
object Expr{
  case class Num(i: Int) extends Expr{ def eval() = i }
  case class NumByte(i: Byte) extends Expr{ def eval() = i.toInt }
  case class UDF(e: Expr, f: Int => Int) extends Expr{ def eval() = f(e.eval()) }
  case class Add(l: Expr, r: Expr) extends Expr{ def eval() = l.eval() + r.eval() }
  case class Sub(l: Expr, r: Expr) extends Expr{ def eval() = l.eval() - r.eval() }
  case class Mul(l: Expr, r: Expr) extends Expr{ def eval() = l.eval() * r.eval() }
  case class Div(l: Expr, r: Expr) extends Expr{ def eval() = l.eval() / r.eval() }
}
object Main {
  def main(args: Array[String]): Unit = {
    import Expr._
    // sqrt(b^2 - 4ac), b = 10, a = 5, c = 17
    val (a, b, c) = (5, 6, 1)
    val expr = UDF(
      Sub(Mul(Num(b), Num(b)), Mul(Mul(NumByte(4), Num(a)), Num(c))),
      math.sqrt(_).toInt
    )
    val determinant = expr.eval()
    println("Hello World " + determinant)
    val determinant2 = devirtualize(expr, "eval")()
    println("Hello World Jitted " + determinant2)
  }

  def isTrivial(cn: ClassNode, mins: MethodInsnNode) = {

    val mn = cn.methods.asScala.find(m => m.name == mins.name && m.desc == mins.desc).get
    val filtered = mn.instructions.iterator().asScala.toVector.filter{
      case _: LabelNode => false
      case _: LineNumberNode => false
      case _ => true
    }

    val res = filtered match {
      case Seq(ins1: VarInsnNode, ins2: FieldInsnNode, ins3: InsnNode) =>
        (ins1.`var` == 0 && ins1.getOpcode == Opcodes.ALOAD) &&
        (ins3.getOpcode >= Opcodes.IRETURN && ins3.getOpcode <= Opcodes.RETURN) &&
        (mn.access & Opcodes.ACC_STATIC) == 0 &&
        mins.desc.startsWith("()")
      case _ => false
    }
    res
  }


  def recurse(self: Object, cn: ClassNode, methodName: String, out: MethodVisitor, newConst: Object => Any): Unit = {
//    println("Recurse " + self.getClass)
    val mn = cn.methods.asScala.find(_.name == methodName).get
    val buffer = mutable.Buffer.empty[AbstractInsnNode]
    for(ins <- mn.instructions.iterator().asScala) {
      def flush(): Unit = {
        buffer.append(ins)
        buffer.foreach{
          case x: VarInsnNode if x.`var` == 0 && x.getOpcode == Opcodes.ALOAD =>
            out.visitLdcInsn(newConst(self))
            out.visitTypeInsn(Opcodes.CHECKCAST, self.getClass.getName.replace('.', '/'))
          case ins3 if ins3.getOpcode >= Opcodes.IRETURN && ins3.getOpcode <= Opcodes.RETURN => //skip
          case x =>
            //            println(x)
            x.accept(out)
        }
        buffer.clear()
      }
      buffer.length match{
        case 0 => (ins, ins.getOpcode) match{
          case (vins: VarInsnNode, Opcodes.ALOAD) if vins.`var` == 0 => buffer.append(ins)
          case _ => flush()
        }
        case 1 => (ins, ins.getOpcode) match{
          case (mins: MethodInsnNode, Opcodes.INVOKEVIRTUAL) if isTrivial(cn, mins) => buffer.append(ins)
          case _ => flush()

        }
        case 2 => (ins, ins.getOpcode) match{
          case (mins: MethodInsnNode, Opcodes.INVOKEINTERFACE)  =>
            val selected = self.getClass.getMethod(buffer(1).asInstanceOf[MethodInsnNode].name).invoke(self)

            recurse(
              selected,
              loadClass(selected),
              methodName,
              out,
              newConst
            )
            buffer.clear()
          case _ =>
            val m = self.getClass.getMethod(buffer(1).asInstanceOf[MethodInsnNode].name)
            val selected = m.invoke(self)


            if (!m.getReturnType.isPrimitive){
              out.visitLdcInsn(newConst(selected))

              out.visitTypeInsn(
                Opcodes.CHECKCAST,
                Type.getType(buffer(1).asInstanceOf[MethodInsnNode].desc).getReturnType.getDescriptor
              )
            }else{
              out.visitLdcInsn(selected)
            }
            buffer.clear()
            (ins, ins.getOpcode) match{
              case (vins: VarInsnNode, Opcodes.ALOAD) if vins.`var` == 0 => buffer.append(ins)
              case _ => flush()

            }
        }
      }
    }
  }

  val loadedClasses = mutable.Map.empty[Class[_], ClassNode]
  def loadClass(self: Any) = {
    loadedClasses.getOrElseUpdate(
      self.getClass,
      {
        val classFile = "/"+self.getClass.getName.replace('.', '/') + ".class"
        val res = getClass.getResourceAsStream(classFile)
        val rawBytes = new Array[Byte](res.available())
        res.read(rawBytes)
        res.close()
        val cr = new ClassReader(rawBytes)
        val cw = new ClassWriter(0)
        val cn = new ClassNode()
        cr.accept(cn, 0)
        cn

      }
    )
  }
  def transform(self: Object, methodName: String) = {
    val cw = new ClassWriter(0 /*ClassWriter.COMPUTE_MAXS*/)

    cw.visit(
      Opcodes.V1_8,
      Opcodes.ACC_PUBLIC,
      "Hello",
      null,
      Type.getInternalName(classOf[Object]),
      null
    )

    val mainMethod = cw.visitMethod(
      Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC,
      "main",
      "()I",
      null,
      null
    )

    val cachedSelfPlaceholders = mutable.Map.empty[Object, String]
    val cachedSelfIndices = mutable.Map.empty[Object, Int]

    recurse(
      self, loadClass(self), methodName, mainMethod,
      self =>
        if (cachedSelfPlaceholders.contains(self)) cachedSelfPlaceholders(self)
        else{
          val placeholder = "CONSTANT_PLACEHOLDER_" + cachedSelfPlaceholders.size
          cachedSelfPlaceholders(self) = placeholder
          cachedSelfIndices(self) = cw.newConst(placeholder)
          placeholder
        }
    )
    mainMethod.visitInsn(Opcodes.IRETURN)
    mainMethod.visitMaxs(100, 100)
    mainMethod.visitEnd()

    val patches =
      if (cachedSelfIndices.isEmpty) null
      else new Array[Object](cachedSelfIndices.valuesIterator.max + 1)
    for((k, i) <- cachedSelfIndices) patches(i) = k
    (cw.toByteArray, patches)
  }

  def devirtualize(interpreted: Object, entrypoint: String): () => Int = {

    val (transformedBytes, patches) = transform(interpreted, entrypoint)

    val field = Class.forName("sun.misc.Unsafe").getDeclaredField("theUnsafe")
    field.setAccessible(true)
    val unsafe = field.get(null).asInstanceOf[sun.misc.Unsafe]
    java.nio.file.Files.write(
      java.nio.file.Paths.get("Hello.class"),
      transformedBytes
    )
    val anon = unsafe.defineAnonymousClass(getClass, transformedBytes, patches)
    () => anon.getMethod("main").invoke(null).asInstanceOf[Int]
  }
}

class Dummy extends Function0[Int] {
  def apply() = 1337
  //    val res = getClass.getResourceAsStream("/autojit/Dummy.class")
  //    val out = new Array[Byte](res.available())
  //    res.read(out)
  //    res.close()
}
