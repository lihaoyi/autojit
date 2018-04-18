package autojit
import utest._
sealed trait Simple{ def eval(): Int }
object Simple{
  case class Num(i: Int) extends Simple{ def eval() = i }
  case class NumByte(i: Byte) extends Simple{ def eval() = i.toInt }
  case class UDF(e: Simple, f: Int => Int) extends Simple{ def eval() = f(e.eval()) }
  case class Add(l: Simple, r: Simple) extends Simple{ def eval() = l.eval() + r.eval() }
  case class Sub(l: Simple, r: Simple) extends Simple{ def eval() = l.eval() - r.eval() }
  case class Mul(l: Simple, r: Simple) extends Simple{ def eval() = l.eval() * r.eval() }
  case class Div(l: Simple, r: Simple) extends Simple{ def eval() = l.eval() / r.eval() }
}

object SimpleTests extends TestSuite {
  import Simple._
  def check0() = {
    val expr = Num(1)
    val determinant = expr.eval()
    val determinant2 = Lib.devirtualize[Simple](expr, "eval").eval()
    assert(
      determinant == 1,
      determinant2 == 1
    )

  }
  def check(a: Int, b: Int, c: Int, expected: Int) = {
    // sqrt(b^2 - 4ac), b = 10, a = 5, c = 17
    val expr = UDF(
      Sub(
        Mul(Num(b), Num(b)),
        Mul(Mul(NumByte(4), Num(a)), Num(c))),
      math.sqrt(_).toInt
    )
    val determinant = expr.eval()
    val determinant2 = Lib.devirtualize[Simple](expr, "eval").eval()
    assert(
      determinant == expected,
      determinant2 == expected
    )

  }
  def tests = Tests{
    'hello0 - check0()
    'hello1 - check(5, 6, 1, 4)
    'hello2 - check(2, 5, 3, 1)
    'hello3 - check(1, 4, 4, 0)
  }
}