package autojit

import utest._
sealed trait Computation{
  def eval(args: Array[Int]): Int
}
object Computation{
  case class Val(i: Int) extends Computation{
    def eval(args: Array[Int]) = i
  }
  case class Var(i: Int) extends Computation{
    def eval(args: Array[Int]) = args(i)
  }
  case class UDF(e: Computation, f: Int => Int) extends Computation{
    def eval(args: Array[Int]) = f(e.eval(args))
  }
  case class Add(l: Computation, r: Computation) extends Computation{
    def eval(args: Array[Int]) = l.eval(args) + r.eval(args)
  }
  case class Sub(l: Computation, r: Computation) extends Computation{
    def eval(args: Array[Int]) = l.eval(args) - r.eval(args)
  }
  case class Mul(l: Computation, r: Computation) extends Computation{
    def eval(args: Array[Int]) = l.eval(args) * r.eval(args)
  }
  case class Div(l: Computation, r: Computation) extends Computation{
    def eval(args: Array[Int]) = l.eval(args) / r.eval(args)
  }
}

object ComputationTests extends TestSuite {
//  def main(args: Array[String]): Unit ={
  def tests = Tests{
    import Computation._
    'hello0 - {
      val expr = Var(1)
      val determinant = expr.eval(Array(5, 6, 1))
      val determinant2 = Lib.devirtualize[Computation](expr, "eval").eval(Array(5, 6, 1))
      assert(
        determinant == 6,
        determinant2 == 6
      )
    }
    'hello1 - {
      val expr = Mul(Var(1), Val(2))
      val determinant = expr.eval(Array(5, 6, 1))
      val determinant2 = Lib.devirtualize[Computation](expr, "eval").eval(Array(5, 6, 1))
      assert(
        determinant == 12,
        determinant2 == 12
      )
    }
    'hello2 - {
      // sqrt(b^2 - 4ac), b = 10, a = 5, c = 17

      val expr = UDF(
        Sub(
          Mul(Var(1), Var(1)),
          Mul(Mul(Val(4), Var(0)), Var(2))),
        math.sqrt(_).toInt
      )
      val determinant = expr.eval(Array(5, 6, 1))
      val determinant2 = Lib.devirtualize[Computation](expr, "eval").eval(Array(5, 6, 1))
      assert(
        determinant == 4,
        determinant2 == 4
      )
    }
    'hello3 - {
      // (-b - sqrt(b^2 - 4ac)) / 2a, b = 10, a = 5, c = 17

      val expr = Div(
        Sub(
          Mul(Val(-1), Var(1)),
          UDF(
            Sub(
              Mul(Var(1), Var(1)),
              Mul(Mul(Val(4), Var(0)), Var(2))),
            math.sqrt(_).toInt
          )
        ),
        Mul(Val(2), Var(0))
      )
      val determinant = expr.eval(Array(2, 5, -3))
      val determinant2 = Lib.devirtualize[Computation](expr, "eval").eval(Array(2, 5, -3))
      assert(
        determinant == -3,
        determinant2 == -3
      )

//      var total = 0
//      val input = Array(2, 5, -3)
//      val start1 = System.currentTimeMillis()
//      var count1 = 0
//      while(System.currentTimeMillis() - start1 < 10000){
//        total += expr.eval(input)
//        total += expr.eval(input)
//        total += expr.eval(input)
//        total += expr.eval(input)
//        total += expr.eval(input)
//        count1 += 1
//      }
//
//      val jitted = Lib.devirtualize[Computation](expr, "eval")
//      val start2 = System.currentTimeMillis()
//      var count2 = 0
//      while(System.currentTimeMillis() - start2 < 10000){
//        total += jitted.eval(input)
//        total += jitted.eval(input)
//        total += jitted.eval(input)
//        total += jitted.eval(input)
//        total += jitted.eval(input)
//        count2 += 1
//      }
//      (count1, count2, total)
    }
  }
}
