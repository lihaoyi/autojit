package autojit

import utest._
sealed trait Failure{ def eval(): Int }
object Failure{
  case class Num(i: Int) extends Failure{
    // This fails because we aren't smart enough to handle the InvokeDynamic
    // call, which references `this` in order to call the bootstrap method
    def eval() = Some(1).map(_ => i).get
  }
  case class Inc(l: Failure) extends Failure{
    def eval() = l.eval() + 1
  }
}

object FailureTests extends TestSuite {
  import Failure._
  def tests = Tests{
    'hello0 - {
      intercept[java.lang.NoClassDefFoundError]{
        Lib.devirtualize[Failure](Num(1), "eval").eval()
      }
    }
  }
}