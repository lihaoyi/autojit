package autojit

sealed trait Expr{
  def eval(): Int
}
object Expr{
  case class Num(i: Int) extends Expr{
    def eval() = i
  }
  case class Add(l: Expr, r: Expr) extends Expr{
    def eval() = l.eval() + r.eval()
  }
  case class Sub(l: Expr, r: Expr) extends Expr{
    def eval() = l.eval() - r.eval()
  }
  case class Mul(l: Expr, r: Expr) extends Expr{
    def eval() = l.eval() * r.eval()
  }
  case class Div(l: Expr, r: Expr) extends Expr{
    def eval() = l.eval() / r.eval()
  }
}
object Main {
  def main(args: Array[String]): Unit = {
    import Expr._
    // b^2 - 4ac, b = 10, a = 5, c = 17
    val a = 5
    val b = 6
    val c = 1
    val expr = Sub(Mul(Num(b), Num(b)), Mul(Mul(Num(4), Num(a)), Num(c)))
    val determinant = expr.eval()
    println("Hello World2 " + determinant)
    val determinant2 = devirtualize(expr, "eval")()
    println("Hello World2 " + determinant2)
  }
  def devirtualize(interpreted: Any, entrypoint: String): () => Int = {
    val field = Class.forName("sun.misc.Unsafe").getDeclaredField("theUnsafe")
    field.setAccessible(true)
    val unsafe = field.get(null).asInstanceOf[sun.misc.Unsafe]
    val res = getClass.getResourceAsStream("/autojit/Dummy.class")
    val out = new Array[Byte](res.available())
    res.read(out)
    res.close()
    val anon = unsafe.defineAnonymousClass(getClass, out, new Array[Object](0))
    anon.newInstance().asInstanceOf[() => Int]
  }
}

class Dummy extends Function0[Int] {
  def apply() = 1337
}