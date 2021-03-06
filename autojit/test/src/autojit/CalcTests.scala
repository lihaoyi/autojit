package autojit
import utest._
sealed trait Calc{ def eval(): Int }
object Calc{
  case class Num(i: Int) extends Calc{ def eval() = i }
  case class NumByte(i: Byte) extends Calc{ def eval() = i.toInt }
  case class UDF(e: Calc, f: Int => Int) extends Calc{ def eval() = f(e.eval()) }
  case class Add(l: Calc, r: Calc) extends Calc{ def eval() = l.eval() + r.eval() }
  case class Sub(l: Calc, r: Calc) extends Calc{ def eval() = l.eval() - r.eval() }
  case class Mul(l: Calc, r: Calc) extends Calc{ def eval() = l.eval() * r.eval() }
  case class Div(l: Calc, r: Calc) extends Calc{ def eval() = l.eval() / r.eval() }
}

object CalcTests extends TestSuite {
  import Calc._
  def check0() = {
    val expr = Num(1)
    val determinant = expr.eval()
    val determinant2 = Lib.devirtualize[Calc](expr, "eval").eval()
    assert(
      determinant == 1,
      determinant2 == 1
    )

  }
  def check1() = {
    val expr = Mul(Num(3), Num(2))
    val determinant = expr.eval()
    val determinant2 = Lib.devirtualize[Calc](expr, "eval").eval()
    assert(
      determinant == 6,
      determinant2 == 6
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
    val determinant2 = Lib.devirtualize[Calc](expr, "eval").eval()
    assert(
      determinant == expected,
      determinant2 == expected
    )

  }
  def tests = Tests{
    'hello0 - check0()
    'hello1 - check1()
    'hello2 - check(5, 6, 1, 4)
    'hello3 - check(2, 5, 3, 1)
    'hello4 - check(1, 4, 4, 0)
  }
}