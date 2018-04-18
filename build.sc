import mill._, mill.scalalib._

object autojit extends ScalaModule{
  def scalaVersion = "2.12.4"
  def ivyDeps = Agg(
    ivy"org.ow2.asm:asm:6.1.1",
    ivy"org.ow2.asm:asm-tree:6.1.1"
  )
  object test extends Tests{
    def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.6.4")
    def testFrameworks = Seq("utest.runner.Framework")
  }
//  def forkArgs = Seq("-Xverify:none")
}
