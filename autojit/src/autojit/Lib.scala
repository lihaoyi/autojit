package autojit

import java.io.{PrintWriter, StringWriter}
import java.lang.invoke.{MethodHandles, MethodType}

import org.objectweb.asm._
import org.objectweb.asm.tree._
import org.objectweb.asm.tree.analysis.{Analyzer, BasicInterpreter}
import org.objectweb.asm.util.{CheckClassAdapter, Textifier, TraceMethodVisitor}

import collection.JavaConverters._
import scala.collection.mutable
import scala.reflect.ClassTag

object Lib {
  def isTrivial(cn: ClassNode, mins: MethodInsnNode) = {
    cn.methods.asScala.find(m => m.name == mins.name && m.desc == mins.desc) match{
      case None => false
      case Some(mn) =>

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
  }
  def recurse(self: Object,
              cn: ClassNode,
              methodName: String,
              className: String,
              withOut: (MethodVisitor => Unit) => String,
              newConst: Object => Any,
              loadClass: Any => ClassNode): String = withOut{ out =>
    out.visitCode()
    self match{
      case self: Intrinsics.Lambda[_] =>
        val mn = cn.methods.asScala.find(_.name == methodName).get
        val df = new Dataflow(false, isTrivial(cn, _), className)
        val analyzer = new Analyzer(df)
        analyzer.analyze(cn.name, mn)
        val frames = analyzer.getFrames

        val inlinedConcreteInsns = for{
          (_, box) <- df.metadata.toSet
          src <- box.inlineable
        } yield src
        val bodyMethodName = recurse(self.body, loadClass(self.body), methodName, className, withOut, newConst, loadClass)
        var bufferedMethod: java.lang.reflect.Method = null
        var bufferedValue: AnyRef = null
        for((insn, i) <- mn.instructions.iterator().asScala.zipWithIndex) {

          val tt = new Textifier()
//          insn.accept(new TraceMethodVisitor(tt))
//          val pw = new PrintWriter(System.out)
//          tt.print(pw)
//          pw.flush()
//          println(df.metadata.get(insn))
          insn match{
            case i: MethodInsnNode if i.name == "handle" =>
              out.visitLdcInsn(new org.objectweb.asm.Handle(
                Opcodes.H_INVOKESTATIC,
                "generated/Hello",
                bodyMethodName,
                mn.desc,
                false
              ))
            case _ =>
              df.metadata.get(insn) match{
                case None => insn.accept(out)
                case Some(nextFrameTop) =>
                  if (nextFrameTop.self.isDefined) () // do nothing
                  else if (nextFrameTop.concrete.isDefined) {
                    bufferedMethod = self.getClass.getMethod(insn.asInstanceOf[MethodInsnNode].name)
                    bufferedValue = bufferedMethod.invoke(self)
                    if (inlinedConcreteInsns.contains(insn)) () // do nothing
                    else if (bufferedMethod.getReturnType.isPrimitive) out.visitLdcInsn(bufferedValue)
                    else{
                      out.visitLdcInsn(newConst(bufferedValue))
                      out.visitTypeInsn(
                        Opcodes.CHECKCAST,
                        Type.getType(bufferedMethod).getReturnType.getDescriptor
                      )
                    }
                  }else if (nextFrameTop.inlineable.isDefined) {
                    val subMethodName = recurse(bufferedValue, loadClass(bufferedValue), methodName, className, withOut, newConst, loadClass)
                    out.visitMethodInsn(Opcodes.INVOKESTATIC, "generated/Hello", subMethodName, mn.desc, false)
                  }else if (nextFrameTop.fromSelf.isDefined) {
                    throw new Exception("Non-inlined self reference slipped through")
                  }
                  else {

                    insn.accept(
                      new MethodVisitor(Opcodes.ASM6, out) {
                        override def visitVarInsn(opcode: Int, `var`: Int): Unit = {
                          super.visitVarInsn(opcode, `var` - 1)
                        }
                      }
                    )
                  }
              }

          }

        }

      case self: Intrinsics.Mapped[_, _] =>
        val mn = cn.methods.asScala.find(_.name == methodName).get
        val method = self.getClass.getMethods.find(_.getName == methodName).get

        out.visitIntInsn(Opcodes.SIPUSH, self.items.length)
        out.visitTypeInsn(Opcodes.ANEWARRAY, Type.getInternalName(method.getReturnType))
        for((item, itemIndex) <- self.items.zipWithIndex){
          out.visitInsn(Opcodes.DUP)
          out.visitIntInsn(Opcodes.SIPUSH, itemIndex)

          for((p, i) <- method.getParameterTypes.zipWithIndex){

            p match{
              case p if p == classOf[Boolean] => out.visitVarInsn(Opcodes.ILOAD, i)
              case p if p == classOf[Char] => out.visitVarInsn(Opcodes.LLOAD, i)
              case p if p == classOf[Byte] => out.visitVarInsn(Opcodes.ILOAD, i)
              case p if p == classOf[Short] => out.visitVarInsn(Opcodes.ILOAD, i)
              case p if p == classOf[Int] => out.visitVarInsn(Opcodes.ILOAD, i)
              case p if p == classOf[Long] => out.visitVarInsn(Opcodes.LLOAD, i)
              case p if p == classOf[Float] => out.visitVarInsn(Opcodes.FLOAD, i)
              case p if p == classOf[Double] => out.visitVarInsn(Opcodes.DLOAD, i)
              case p  => out.visitVarInsn(Opcodes.ALOAD, i)
            }

          }
          val subMethodName = recurse(item, loadClass(item), methodName, className, withOut, newConst, loadClass)
          out.visitMethodInsn(Opcodes.INVOKESTATIC, "generated/Hello", subMethodName, mn.desc, false)

          method.getReturnType match{
            case p if p == classOf[Boolean] => out.visitInsn(Opcodes.BASTORE)
            case p if p == classOf[Char] => out.visitInsn(Opcodes.CASTORE)
            case p if p == classOf[Byte] => out.visitInsn(Opcodes.BASTORE)
            case p if p == classOf[Short] => out.visitInsn(Opcodes.SASTORE)
            case p if p == classOf[Int] => out.visitInsn(Opcodes.IASTORE)
            case p if p == classOf[Long] => out.visitInsn(Opcodes.LASTORE)
            case p if p == classOf[Float] => out.visitInsn(Opcodes.FASTORE)
            case p if p == classOf[Double] => out.visitInsn(Opcodes.DASTORE)
            case p if p == classOf[Unit] => ???
            case p  => out.visitInsn(Opcodes.AASTORE)
          }
        }

        out.visitVarInsn(Opcodes.ASTORE, 0)
        for (wrapInsn <- cn.methods.asScala.find(_.name == "wrap").get.instructions.iterator().asScala){
          wrapInsn.accept(
            new MethodVisitor(Opcodes.ASM6, out) {
              override def visitVarInsn(opcode: Int, `var`: Int): Unit = {
                super.visitVarInsn(opcode, `var` - 1)
              }
            }
          )
        }

      case self: Intrinsics.Foreach[_] =>
        val mn = cn.methods.asScala.find(_.name == methodName).get
        val method = self.getClass.getMethods.find(_.getName == methodName).get
        for(item <- self.items){
          for((p, i) <- method.getParameterTypes.zipWithIndex){

            p match{
              case p if p == classOf[Boolean] => out.visitVarInsn(Opcodes.ILOAD, i)
              case p if p == classOf[Char] => out.visitVarInsn(Opcodes.LLOAD, i)
              case p if p == classOf[Byte] => out.visitVarInsn(Opcodes.ILOAD, i)
              case p if p == classOf[Short] => out.visitVarInsn(Opcodes.ILOAD, i)
              case p if p == classOf[Int] => out.visitVarInsn(Opcodes.ILOAD, i)
              case p if p == classOf[Long] => out.visitVarInsn(Opcodes.LLOAD, i)
              case p if p == classOf[Float] => out.visitVarInsn(Opcodes.FLOAD, i)
              case p if p == classOf[Double] => out.visitVarInsn(Opcodes.DLOAD, i)
              case p  => out.visitVarInsn(Opcodes.ALOAD, i)
            }

          }
          val subMethodName = recurse(item, loadClass(item), methodName, className, withOut, newConst, loadClass)
          out.visitMethodInsn(Opcodes.INVOKESTATIC, "generated/Hello", subMethodName, mn.desc, false)

        }
        method.getReturnType match{
          case p if p == classOf[Boolean] => out.visitInsn(Opcodes.IRETURN)
          case p if p == classOf[Char] => out.visitInsn(Opcodes.IRETURN)
          case p if p == classOf[Byte] => out.visitInsn(Opcodes.IRETURN)
          case p if p == classOf[Short] => out.visitInsn(Opcodes.IRETURN)
          case p if p == classOf[Int] => out.visitInsn(Opcodes.IRETURN)
          case p if p == classOf[Long] => out.visitInsn(Opcodes.LRETURN)
          case p if p == classOf[Float] => out.visitInsn(Opcodes.FRETURN)
          case p if p == classOf[Double] => out.visitInsn(Opcodes.DRETURN)
          case p if p == classOf[Unit] => out.visitInsn(Opcodes.RETURN)
          case p  => out.visitInsn(Opcodes.ARETURN)
        }

      case _ =>
        val mn = cn.methods.asScala.find(_.name == methodName).get
        val df = new Dataflow(false, isTrivial(cn, _), className)
        val analyzer = new Analyzer(df)
        analyzer.analyze(cn.name, mn)
        val frames = analyzer.getFrames

        val inlinedConcreteInsns = for{
          (_, box) <- df.metadata.toSet
          src <- box.inlineable
        } yield src

        var bufferedMethod: java.lang.reflect.Method = null
        var bufferedValue: AnyRef = null
        for((insn, i) <- mn.instructions.iterator().asScala.zipWithIndex) {
          df.metadata.get(insn) match{
            case None => insn.accept(out)
            case Some(nextFrameTop) =>
              if (nextFrameTop.self.isDefined) () // do nothing
              else if (nextFrameTop.concrete.isDefined) {
                bufferedMethod = self.getClass.getMethod(insn.asInstanceOf[MethodInsnNode].name)
                bufferedValue = bufferedMethod.invoke(self) match{
                  case i: java.lang.Byte => Int.box(i.toInt)
                  case i: java.lang.Short => Int.box(i.toInt)
                  case i => i
                }
                if (inlinedConcreteInsns.contains(insn)) () // do nothing
                else if (bufferedMethod.getReturnType.isPrimitive) {
                  out.visitLdcInsn(bufferedValue)
                } else{
                  out.visitLdcInsn(newConst(bufferedValue))
                  out.visitTypeInsn(
                    Opcodes.CHECKCAST,
                    Type.getType(bufferedMethod).getReturnType.getInternalName
                  )
                }
              }else if (nextFrameTop.inlineable.isDefined) {
                val subMethodName = recurse(bufferedValue, loadClass(bufferedValue), methodName, className, withOut, newConst, loadClass)
                out.visitMethodInsn(Opcodes.INVOKESTATIC, "generated/Hello", subMethodName, mn.desc, false)
              }else if (nextFrameTop.fromSelf.isDefined) {
                throw new Exception("Non-inlined self reference slipped through")
              }
              else {

                insn.accept(
                  new MethodVisitor(Opcodes.ASM6, out) {
                    override def visitVarInsn(opcode: Int, `var`: Int): Unit = {
                      super.visitVarInsn(opcode, `var` - 1)
                    }
                  }
                )
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
    val cw0 = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    val cw = new CheckClassAdapter(cw0)

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
    constructor.visitMaxs(100, 100)
    val bi = new BasicInterpreter()


    constructor.visitEnd()

    val cachedSelfPlaceholders = mutable.Map.empty[Object, String]
    val cachedSelfIndices = mutable.Map.empty[Object, Int]

    val loadedClasses = mutable.Map.empty[Class[_], Array[Byte]]
    def loadClass(self: Any) = {
      val rawBytes = loadedClasses.getOrElseUpdate(
        self.getClass,
        {
          val classFile = "/"+self.getClass.getName.replace('.', '/') + ".class"
          val res = getClass.getResourceAsStream(classFile)
          val rawBytes = new Array[Byte](res.available())
          res.read(rawBytes)
          res.close()
          rawBytes
        }
      )
      val cr = new ClassReader(rawBytes)
      val cn = new ClassNode()
      cr.accept(cn, ClassReader.SKIP_FRAMES)
      cn
    }
    val desc = getMethodDescriptor(method)
    val superClassName = Type.getInternalName(superClass)
    val mainMethod = cw.visitMethod(
      Opcodes.ACC_PUBLIC | Opcodes.ACC_FINAL,
      method.getName,
      desc,
      null,
      null
    )
    val start = new Label()
    val end = new Label()

    mainMethod.visitCode()
    mainMethod.visitLabel(start)

    for((p, i) <- method.getParameterTypes.zipWithIndex){

      p match{
        case p if p == classOf[Boolean] => mainMethod.visitVarInsn(Opcodes.ILOAD, i + 1)
        case p if p == classOf[Char] => mainMethod.visitVarInsn(Opcodes.LLOAD, i + 1)
        case p if p == classOf[Byte] => mainMethod.visitVarInsn(Opcodes.ILOAD, i + 1)
        case p if p == classOf[Short] => mainMethod.visitVarInsn(Opcodes.ILOAD, i + 1)
        case p if p == classOf[Int] => mainMethod.visitVarInsn(Opcodes.ILOAD, i + 1)
        case p if p == classOf[Long] => mainMethod.visitVarInsn(Opcodes.LLOAD, i + 1)
        case p if p == classOf[Float] => mainMethod.visitVarInsn(Opcodes.FLOAD, i + 1)
        case p if p == classOf[Double] => mainMethod.visitVarInsn(Opcodes.DLOAD, i + 1)
        case p  => mainMethod.visitVarInsn(Opcodes.ALOAD, i + 1)
      }

    }
    mainMethod.visitMethodInsn(Opcodes.INVOKESTATIC, "generated/Hello", method.getName + 0, desc, false)
    method.getReturnType match{
      case p if p == classOf[Boolean] => mainMethod.visitInsn(Opcodes.IRETURN)
      case p if p == classOf[Char] => mainMethod.visitInsn(Opcodes.IRETURN)
      case p if p == classOf[Byte] => mainMethod.visitInsn(Opcodes.IRETURN)
      case p if p == classOf[Short] => mainMethod.visitInsn(Opcodes.IRETURN)
      case p if p == classOf[Int] => mainMethod.visitInsn(Opcodes.IRETURN)
      case p if p == classOf[Long] => mainMethod.visitInsn(Opcodes.LRETURN)
      case p if p == classOf[Float] => mainMethod.visitInsn(Opcodes.FRETURN)
      case p if p == classOf[Double] => mainMethod.visitInsn(Opcodes.DRETURN)
      case p if p == classOf[Unit] => mainMethod.visitInsn(Opcodes.RETURN)
      case p  => mainMethod.visitInsn(Opcodes.ARETURN)
    }
    mainMethod.visitLabel(end)
    mainMethod.visitLocalVariable("this", "Lgenerated/Hello;", null, start, end, 0)

    mainMethod.visitMaxs(100, 100)
    mainMethod.visitEnd()

    var methodCount = 0
    recurse(
      self, loadClass(self), method.getName, superClassName,
      f => {
        val methodName = method.getName + methodCount
        val newMethod = cw.visitMethod(
          Opcodes.ACC_PUBLIC | Opcodes.ACC_FINAL | Opcodes.ACC_STATIC,
          methodName,
          getMethodDescriptor(method),
          null,
          null
        )
        methodCount += 1
        f(new InlineValidator(newMethod, superClassName))
//        val textifier = new Textifier()
//        f(new InlineValidator(new TraceMethodVisitor(newMethod, textifier), superClassName))
//        val pw = new PrintWriter(System.out)
//        println("METHOD " + methodName)
//        textifier.print(pw)
//        pw.flush()

        newMethod.visitMaxs(100, 100)
        newMethod.visitEnd()
        methodName
      },
      self =>
        if (cachedSelfPlaceholders.contains(self)) cachedSelfPlaceholders(self)
        else{
          val placeholder = "CONSTANT_PLACEHOLDER_" + cachedSelfPlaceholders.size
          cachedSelfPlaceholders(self) = placeholder
          cachedSelfIndices(self) = cw0.newConst(placeholder)
          placeholder
        },
      loadClass
    )
    val patches =
      if (cachedSelfIndices.isEmpty) null
      else new Array[Object](cachedSelfIndices.valuesIterator.max + 1)
    for((k, i) <- cachedSelfIndices) patches(i) = k
    (cw0.toByteArray, patches)
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
