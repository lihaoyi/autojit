package autojit

import utest._

import scala.collection.mutable

object SchemeTests extends TestSuite{
  sealed trait Value
  object Value{
    case class Null() extends Value
    case class Num(i: Int) extends Value
    case class Lambda(f: Expr.Lambda, ctx: Array[Value]) extends Value
  }
  sealed trait Expr{
    def eval(ctx: Array[Value]): Value
  }
  object Expr{
    case class Literal(i: Int) extends Expr{
      def eval(ctx: Array[Value]) = Value.Num(i)
    }
    case class Ident(i: Int) extends Expr{
      def eval(ctx: Array[Value]) = ctx(i)
    }
    case class Add(a: Expr, b: Expr) extends Expr{
      def eval(ctx: Array[Value]) = {
        Value.Num(a.eval(ctx).asInstanceOf[Value.Num].i + b.eval(ctx).asInstanceOf[Value.Num].i)
      }
    }
    case class Sub(a: Expr, b: Expr) extends Expr{
      def eval(ctx: Array[Value]) = {
        Value.Num(a.eval(ctx).asInstanceOf[Value.Num].i - b.eval(ctx).asInstanceOf[Value.Num].i)
      }
    }
    case class Zero(a: Expr) extends Expr{
      def eval(ctx: Array[Value]) = {
        if (a.eval(ctx) == Value.Num(0)) Value.Num(1) else Value.Num(0)
      }
    }
    case class If(a: Expr, b: Expr, c: Expr) extends Expr{
      def eval(ctx: Array[Value]) = {
        val p = a.eval(ctx)
        val res = if (p != Value.Num(0)) b.eval(ctx) else c.eval(ctx)
        res
      }
    }
    case class Apply(items: IndexedSeq[Expr]) extends Expr{
      def eval(ctx: Array[Value]) = {
        val f = items(0).eval(ctx).asInstanceOf[Value.Lambda]
        val newCtx = f.ctx.clone()
        for((b, i) <- f.f.bindings.zipWithIndex){
          val value = items(i + 1).eval(ctx)
          newCtx(b) = value
        }
        f.f.body.eval(newCtx)
      }
    }
    case class Lambda(bindings: Seq[Int], body: Expr) extends Expr{
      def eval(ctx: Array[Value]) = Value.Lambda(this, ctx)
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
              Expr.Lambda(popped(1).asInstanceOf[Expr.Apply].items.map(_.asInstanceOf[Expr.Ident].i), popped(2))
            } else if (popped.head == Expr.Ident(mapping("+"))) {
              Expr.Add(popped(1), popped(2))
            } else if (popped.head == Expr.Ident(mapping("-"))) {
              Expr.Sub(popped(1), popped(2))
            } else if (popped.head == Expr.Ident(mapping("if"))) {
              Expr.If(popped(1), popped(2), popped(3))
            } else if (popped.head == Expr.Ident(mapping("zero?"))) {
              Expr.Zero(popped(1))
            } else {
              Expr.Apply(popped)
            }
          )
        }
      }

      (mapping.size, stack(0)(0))
    }
    def eval(str: String) = {
      val (locals, expr) = parse(str)
      expr.eval(Array.fill(locals)(Value.Null()))
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
    'fib - {
      // Test case taken from https://stackoverflow.com/a/15057955/871202
      val fib10 =
        eval("""(((lambda (x) (x x))
              |  (lambda (fib-gen)
              |    (lambda (it second first)
              |      (if (zero? it)
              |          first
              |          ((fib-gen fib-gen) (- it 1) (+ first second) second)))))
              | 10 1 0)""".stripMargin)
      assert(fib10 == Value.Num(55))
    }
  }
}