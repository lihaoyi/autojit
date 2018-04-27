package autojit

import java.lang.invoke.{MethodHandle, MethodHandles, MethodType}

import utest._

import scala.collection.mutable

object SchemeTests extends TestSuite{
  sealed trait Value
  object Value{
    case class Null() extends Value
    case class Arr(values: Seq[Value]) extends Value
    case class Num(i: Int) extends Value
    case class Lambda(bindings: Seq[Int], eval: MethodHandle, ctx: Array[Value]) extends Value
  }
  sealed trait Expr{
    def eval(ctx: Array[Value]): Value
  }
  object Expr{
    case class Literal(i: Int) extends Expr{
      def eval(ctx: Array[Value]): Value = Value.Num(i)
    }
    case class Ident(i: Int) extends Expr{
      def eval(ctx: Array[Value]): Value = ctx(i)
    }
    case class Add(a: Expr, b: Expr) extends Expr{
      def eval(ctx: Array[Value]): Value = {
        Value.Num(a.eval(ctx).asInstanceOf[Value.Num].i + b.eval(ctx).asInstanceOf[Value.Num].i)
      }
    }
    case class Sub(a: Expr, b: Expr) extends Expr{
      def eval(ctx: Array[Value]): Value = {
        Value.Num(a.eval(ctx).asInstanceOf[Value.Num].i - b.eval(ctx).asInstanceOf[Value.Num].i)
      }
    }
    case class Zero(a: Expr) extends Expr{
      def eval(ctx: Array[Value]): Value = {
        if (a.eval(ctx) == Value.Num(0)) Value.Num(1) else Value.Num(0)
      }
    }
    case class If(a: Expr, b: Expr, c: Expr) extends Expr{
      def eval(ctx: Array[Value]): Value = {
        if (a.eval(ctx) != Value.Num(0)) b.eval(ctx) else c.eval(ctx)
      }
    }
    case class Apply(f: Expr, items: Expr) extends Expr{
      def eval(ctx: Array[Value]): Value = {
        val funcValue = f.eval(ctx).asInstanceOf[Value.Lambda]
        val newCtx = funcValue.ctx.clone()
        val args = items.eval(ctx).asInstanceOf[Value.Arr].values
        for((b, i) <- funcValue.bindings.zipWithIndex){
          newCtx(b) = args(i)
        }
        val res = funcValue.eval.invoke(newCtx)
        res.asInstanceOf[Value]
      }
    }
    case class ApplyArgs(items: IndexedSeq[Expr])
      extends Expr with Intrinsics.Mapped[Expr, Value]{

      def eval(ctx: Array[Value]): Value = wrap(items.toArray.map(_.eval(ctx)))
      def wrap(t: Array[Value]) = Value.Arr(t)
    }
    case class Lambda(bindings: Array[Int], body: Expr)
      extends Expr with Intrinsics.Lambda[Expr]{

      def handle() = MethodHandles.lookup()
        .findVirtual(
          body.getClass,
          "eval",
          MethodType.methodType(classOf[Value], classOf[Array[Value]])
        )
        .bindTo(body)

      def eval(ctx: Array[Value]): Value = Value.Lambda(bindings, handle(), ctx)
    }
  }


  def tests = Tests{
    def parse(input: String): (Int, Expr) = {
      val mapping = mutable.Map.empty[String, Int]
      mapping("lambda") = mapping.size
      mapping("+") = mapping.size
      mapping("-") = mapping.size
      mapping("if") = mapping.size
      mapping("zero?") = mapping.size
      val stack = mutable.ArrayBuffer(mutable.ArrayBuffer.empty[Expr])
      val tokens = input.split("(\n| )+")
      def handleAtom(token: String) = {
        stack.last.append(
          if (token(0).isDigit) Expr.Literal(token.toInt)
          else Expr.Ident(mapping.getOrElseUpdate(token, mapping.size))
        )
      }
      for(token <- tokens){
        var openParens = 0
        while(token(openParens) == '('){
          stack.append(mutable.ArrayBuffer.empty)
          openParens += 1
        }


        var closeParens = 0
        while(token(token.length - 1 - closeParens) == ')'){
          closeParens += 1
        }

        handleAtom(token.drop(openParens).dropRight(closeParens))

        for(i <- 0 until closeParens) {
          val popped = stack.remove(stack.length - 1)
          stack.last.append(
            if (popped.head == Expr.Ident(mapping("lambda"))) {
              Expr.Lambda(
                popped(1).asInstanceOf[Expr.Apply].f.asInstanceOf[Expr.Ident].i +:
                popped(1).asInstanceOf[Expr.Apply].items
                         .asInstanceOf[Expr.ApplyArgs].items
                         .map(_.asInstanceOf[Expr.Ident].i)
                         .toArray,
                popped(2)
              )
            } else if (popped.head == Expr.Ident(mapping("+"))) {
              Expr.Add(popped(1), popped(2))
            } else if (popped.head == Expr.Ident(mapping("-"))) {
              Expr.Sub(popped(1), popped(2))
            } else if (popped.head == Expr.Ident(mapping("if"))) {
              Expr.If(popped(1), popped(2), popped(3))
            } else if (popped.head == Expr.Ident(mapping("zero?"))) {
              Expr.Zero(popped(1))
            } else {
              Expr.Apply(popped(0), Expr.ApplyArgs(popped.drop(1)))
            }
          )
        }
      }

      (mapping.size, stack(0)(0))
    }
    def eval(str: String) = {
      val (locals, expr) = parse(str)
      val res = expr.eval(Array.fill(locals)(Value.Null()))
      val expr2 = Lib.devirtualize[Expr](expr, "eval")
      val res2 = expr2.eval(Array.fill(locals)(Value.Null()))
      assert(res == res2)
      res
    }

    'literal - {
      assert(eval("1") == Value.Num(1))
    }
    'add - {
      assert(eval("(+ (+ 1 2) (+ 3 4))") == Value.Num(10))
    }
    'if - {
      assert(
        eval("(if 1 1 2)") == Value.Num(1),
        eval("(if 0 1 2)") == Value.Num(2)
      )
    }
    'lambda - {
      assert(eval("((lambda (a) a) 12)") == Value.Num(12))
    }
    'lambda2 - {
      assert(eval("((lambda (a b) (+ a b)) 12 13)") == Value.Num(25))
    }
    'lambda3 - {
      assert(eval("((lambda (x y z) (x (+ y z))) (lambda (a) (+ a a)) 1 2)") == Value.Num(6))
    }
    'fib - {
      // Test case taken from https://stackoverflow.com/a/15057955/871202
      val fib10 =
        eval("""(((lambda (x) (x x))
              |  (lambda (fib-gen)
              |    (lambda (it second first)
              |      (if (zero? it)
              |          first
              |          ((fib-gen fib-gen) (- it 1) (+ first second) second)))))
              | 15 1 0)""".stripMargin)
      assert(fib10 == Value.Num(610))
      fib10
    }

//    'perf - {
//      var total = 0
//      val (locals, expr) = parse("""(((lambda (x) (x x))
//                          |  (lambda (fib-gen)
//                          |    (lambda (it second first)
//                          |      (if (zero? it)
//                          |          first
//                          |          ((fib-gen fib-gen) (- it 1) (+ first second) second)))))
//                          | 15 1 0)""".stripMargin)
//      val start1 = System.currentTimeMillis()
//      var count1 = 0
//      while(System.currentTimeMillis() - start1 < 10000){
//        total += expr.eval(Array.fill(locals)(Value.Null())).asInstanceOf[Value.Num].i
//        total += expr.eval(Array.fill(locals)(Value.Null())).asInstanceOf[Value.Num].i
//        total += expr.eval(Array.fill(locals)(Value.Null())).asInstanceOf[Value.Num].i
//        total += expr.eval(Array.fill(locals)(Value.Null())).asInstanceOf[Value.Num].i
//        total += expr.eval(Array.fill(locals)(Value.Null())).asInstanceOf[Value.Num].i
//        count1 += 1
//      }
//
//      val jitted = Lib.devirtualize[Expr](expr, "eval")
//      val start2 = System.currentTimeMillis()
//      var count2 = 0
//      while(System.currentTimeMillis() - start2 < 10000){
//        total += jitted.eval(Array.fill(locals)(Value.Null())).asInstanceOf[Value.Num].i
//        total += jitted.eval(Array.fill(locals)(Value.Null())).asInstanceOf[Value.Num].i
//        total += jitted.eval(Array.fill(locals)(Value.Null())).asInstanceOf[Value.Num].i
//        total += jitted.eval(Array.fill(locals)(Value.Null())).asInstanceOf[Value.Num].i
//        total += jitted.eval(Array.fill(locals)(Value.Null())).asInstanceOf[Value.Num].i
//        count2 += 1
//      }
//      (count1, count2, total)
//    }
  }
}
