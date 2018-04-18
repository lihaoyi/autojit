package autojit
import utest._
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

object SimpleTests extends TestSuite {
//  def main(args: Array[String]): Unit ={
  def tests = Tests{
    'hello - {
      import Expr._
      // sqrt(b^2 - 4ac), b = 10, a = 5, c = 17
      val (a, b, c) = (5, 6, 1)
      val expr = UDF(
        Sub(
          Mul(Num(b), Num(b)),
          Mul(Mul(NumByte(4), Num(a)), Num(c))),
        math.sqrt(_).toInt
      )
      val determinant = expr.eval()
      val determinant2 = Lib.devirtualize(expr, "eval")()
      assert(
        determinant == 4,
        determinant2 == 4
      )
    }
  }
}