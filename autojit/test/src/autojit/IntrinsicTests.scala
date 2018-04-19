package autojit

import utest._
sealed trait Intr{ def eval(out: Int => Unit): Unit }
object Intr{
  case class Num(i: Int) extends Intr{ def eval(out: Int => Unit) = out(i) }
  case class Sum(items: Seq[Intr]) extends Intr with Intrinsics.Foreach[Intr] {
    def eval(out: Int => Unit) = items.foreach(_.eval(out))
  }
  
}

object IntrTests extends TestSuite {
  import Intr._
  def tests = Tests{
    'flat - {
      val tree = Sum(Seq(Num(1), Num(2), Num(3)))
      val jitted = Lib.devirtualize[Intr](tree, "eval")
      var normalOut = 0
      var jittedOut = 0
      tree.eval(normalOut += _)
      jitted.eval(jittedOut += _)
      assert(
        normalOut == 6,
        jittedOut == 6
      )
      (normalOut, jittedOut)
    }
    'nested - {
      val tree = Sum(Seq(Num(1), Sum(Seq(Num(2), Num(3)))))
      val jitted = Lib.devirtualize[Intr](tree, "eval")
      var normalOut = 0
      var jittedOut = 0
      tree.eval(normalOut += _)
      jitted.eval(jittedOut += _)
      assert(
        normalOut == 6,
        jittedOut == 6
      )
      (normalOut, jittedOut)
    }
  }
}