package autojit

import org.objectweb.asm.Opcodes.ASM6
import org.objectweb.asm._
import org.objectweb.asm.tree._
import org.objectweb.asm.tree.analysis.Analyzer

import collection.JavaConverters._
import scala.collection.mutable
import scala.reflect.ClassTag

object Lib {
  def isTrivial(cn: ClassNode, mins: MethodInsnNode) = {

    val mn = cn.methods.asScala.find(m => m.name == mins.name && m.desc == mins.desc).get
    val filtered = mn.instructions.iterator().asScala.toVector.filter{
      case _: LabelNode => false
      case _: LineNumberNode => false
      case _ => true
    }

    filtered match {
      case Seq(ins1: VarInsnNode, ins2: FieldInsnNode, ins3: InsnNode) =>
        (ins1.`var` == 0 && ins1.getOpcode == Opcodes.ALOAD) &&
        (ins3.getOpcode >= Opcodes.IRETURN && ins3.getOpcode <= Opcodes.RETURN) &&
        (mn.access & Opcodes.ACC_STATIC) == 0 &&
        mins.desc.startsWith("()")
      case _ => false
    }
  }


  def recurse(self: Object,
              cn: ClassNode,
              methodName: String,
              className: String,
              out: MethodVisitor,
              newConst: Object => Any,
              loadClass: Any => ClassNode): Unit = {
    println("Recurse " + self.getClass)
    println(methodName)
    val mn = cn.methods.asScala.find(_.name == methodName).get
    val analyzer = new Analyzer(new Dataflow(true, isTrivial(cn, _), className))
    analyzer.analyze(cn.name, mn)
    val frames = analyzer.getFrames

    val inlinedConcreteInsns = for{
      f <- frames
      if f != null
      b <- 0 until f.getStackSize
      src <- f.getStack(b).inlineable
    } yield src

    var bufferedMethod: java.lang.reflect.Method = null
    var bufferedValue: AnyRef = null
    for((insn, i) <- mn.instructions.iterator().asScala.zipWithIndex) {
      pprint.log(insn)
      if (i + 1 < frames.length){
        val nextFrame = frames(i+1)
        pprint.log(nextFrame)
        if (nextFrame != null){

          val nextFrameTop = nextFrame.getStack(nextFrame.getStackSize-1)
          if (nextFrameTop.self) () // do nothing
          else if (nextFrameTop.concrete.isDefined) {
            bufferedMethod = self.getClass.getMethod(insn.asInstanceOf[MethodInsnNode].name)
            bufferedValue = bufferedMethod.invoke(self)
            if (!inlinedConcreteInsns.contains(insn)){
              if (!bufferedMethod.getReturnType.isPrimitive){
                out.visitLdcInsn(newConst(bufferedValue))

                out.visitTypeInsn(
                  Opcodes.CHECKCAST,
                  Type.getType(bufferedMethod).getReturnType.getDescriptor
                )
              }else{
                out.visitLdcInsn(bufferedValue)
              }
            }
          }else if (nextFrameTop.inlineable.isDefined) {
            recurse(bufferedValue, loadClass(bufferedValue), methodName, className, out, newConst, loadClass)
          }else {
            insn.accept(out)
          }
        }
      }
    }
  }

  def getDescriptorForClass(c: Class[_]) = {
    if(c.isPrimitive()){
      if(c == classOf[Byte]) "B"
      else if(c == classOf[Char]) "C"
      else if(c == classOf[Double]) "D"
      else if(c == classOf[Float]) "F"
      else if(c == classOf[Int]) "I"
      else if(c == classOf[Long]) "J"
      else if(c == classOf[Short]) "S"
      else if(c == classOf[Boolean]) "Z"
      else if(c == classOf[Unit]) "V"
    }
    else if(c.isArray()) c.getName().replace('.', '/')
    else ('L'+c.getName()+';').replace('.', '/')
  }

  def getMethodDescriptor(m: java.lang.reflect.Method) = {
    var s = "("
    for(c <- m.getParameterTypes()) s += getDescriptorForClass(c)
    s += ')'
    s + getDescriptorForClass(m.getReturnType())
  }

  def transform(self: Object, superClass: Class[_], method: java.lang.reflect.Method) = {
    val cw = new ClassWriter(0 /*ClassWriter.COMPUTE_MAXS*/)

    cw.visit(
      Opcodes.V1_8,
      Opcodes.ACC_PUBLIC,
      "generated/Hello",
      null,
      Type.getInternalName(classOf[Object]),
      Array(Type.getInternalName(superClass))
    )

    val constructor = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null)

    constructor.visitCode()
    constructor.visitVarInsn(Opcodes.ALOAD, 0)
    constructor.visitMethodInsn(
      Opcodes.INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false
    )
    constructor.visitInsn(Opcodes.RETURN)
    constructor.visitMaxs(1,1)
    constructor.visitEnd()

    val mainMethod = cw.visitMethod(
      Opcodes.ACC_PUBLIC,
      method.getName,
      getMethodDescriptor(method),
      null,
      null
    )

    val cachedSelfPlaceholders = mutable.Map.empty[Object, String]
    val cachedSelfIndices = mutable.Map.empty[Object, Int]

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

    val superClassName = Type.getInternalName(superClass)
    recurse(
      self, loadClass(self), method.getName, superClassName,
      new InlineValidator(mainMethod, superClassName),
      self =>
        if (cachedSelfPlaceholders.contains(self)) cachedSelfPlaceholders(self)
        else{
          val placeholder = "CONSTANT_PLACEHOLDER_" + cachedSelfPlaceholders.size
          cachedSelfPlaceholders(self) = placeholder
          cachedSelfIndices(self) = cw.newConst(placeholder)
          placeholder
        },
      loadClass
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

  def devirtualize[T: ClassTag](interpreted: T, entrypoint: String): T = {

    val (transformedBytes, patches) = transform(
      interpreted.asInstanceOf[AnyRef],
      implicitly[ClassTag[T]].runtimeClass,
      implicitly[ClassTag[T]].runtimeClass.getMethods.find(_.getName == entrypoint).get
    )

    val field = Class.forName("sun.misc.Unsafe").getDeclaredField("theUnsafe")
    field.setAccessible(true)
    val unsafe = field.get(null).asInstanceOf[sun.misc.Unsafe]
    java.nio.file.Files.write(
      java.nio.file.Paths.get("Hello.class"),
      transformedBytes
    )
    val anon = unsafe.defineAnonymousClass(classOf[Function1[_, _]], transformedBytes, patches)
    anon.newInstance().asInstanceOf[T]
  }
}
