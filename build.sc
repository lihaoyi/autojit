import mill._, mill.scalalib._

object autojit extends ScalaModule{
  def scalaVersion = "2.12.4"
  def ivyDeps = Agg(
    ivy"org.ow2.asm:asm:6.1.1"
  )
}
