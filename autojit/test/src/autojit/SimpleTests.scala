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
  def check(a: Int, b: Int, c: Int, expected: Int) = {
    import Expr._
    // sqrt(b^2 - 4ac), b = 10, a = 5, c = 17
    val expr = UDF(
      Sub(
        Mul(Num(b), Num(b)),
        Mul(Mul(NumByte(4), Num(a)), Num(c))),
      math.sqrt(_).toInt
    )
    val determinant = expr.eval()
    val determinant2 = Lib.devirtualize[Expr](expr, "eval").eval()
    assert(
      determinant == expected,
      determinant2 == expected
    )

  }
  def tests = Tests{
    'hello1 - check(5, 6, 1, 4)
    'hello2 - check(2, 5, 3, 1)
    'hello3 - check(1, 4, 4, 0)
  }
}