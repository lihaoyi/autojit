package autojit

import utest._

import scala.collection.mutable

object SchemeTests extends TestSuite{
  sealed trait Value
  object Value{
    case class Num(i: Int) extends Value
    case class Lambda(f: Expr.Lambda) extends Value
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
    case class Apply(items: IndexedSeq[Expr]) extends Expr{
      def eval(ctx: Array[Value]) = {
        val f = items(0).eval(ctx).asInstanceOf[Value.Lambda].f
        val newCtx = ctx.clone()
        newCtx(f.binding) = items(1).eval(ctx)
        f.body.eval(newCtx)
      }
    }
    case class Lambda(binding: Int, body: Expr) extends Expr{
      def eval(ctx: Array[Value]) = Value.Lambda(this)
    }
  }


  def tests = Tests{
    def parse(input: String): Expr = {
      val mapping = mutable.Map.empty[String, Int]
      mapping("lambda") = 0
      val stack = mutable.ArrayBuffer(mutable.ArrayBuffer.empty[Expr])
      val tokens = input.split(' ')
      def handleAtom(token: String) = {
        println("handleAtom " + token)
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
            if (popped.head != Expr.Ident(mapping("lambda"))) Expr.Apply(popped)
            else Expr.Lambda(popped(1).asInstanceOf[Expr.Ident].i, popped(2))
          )
        }
      }

      stack(0)(0)
    }
    'literal - {
      assert(parse("1").eval(Array()) == Value.Num(1))
    }
    'lambda - {
      assert(parse("((lambda a a) 12)").eval(Array(Value.Num(0), Value.Num(0))) == Value.Num(12))
    }
  }
}
