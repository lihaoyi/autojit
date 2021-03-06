package autojit

import utest._
sealed trait Failure{ def eval(): Int }
object Failure{
  case class Lambda(i: Int) extends Failure{
    // This fails because we aren't smart enough to handle the InvokeDynamic
    // call, which references `this` in order to call the bootstrap method
    def eval() = Some(1).map(_ => i).get
  }
  case class External(i: Int) extends Failure{
    // This fails because we aren't smart enough to handle the InvokeDynamic
    // call, which references `this` in order to call the bootstrap method
    def eval() = external(this)
  }
  case class Inc(l: Failure) extends Failure{
    def eval() = l.eval() + 1
  }

  def external(x: Any) = {
    println(x)
    1337
  }
}

object FailureTests extends TestSuite {
  import Failure._
  def tests = Tests{
    'hello0 - {
      val ex = intercept[java.lang.Exception]{
        Lib.devirtualize[Failure](Lambda(1), "eval").eval()
      }
      assert(ex.getMessage == "Non-inlined self reference slipped through")
    }
    'hello1 - {
      val ex = intercept[java.lang.Exception]{
        Lib.devirtualize[Failure](External(1), "eval").eval()
      }
      assert(ex.getMessage == "Non-inlined self reference slipped through")
    }
  }
}