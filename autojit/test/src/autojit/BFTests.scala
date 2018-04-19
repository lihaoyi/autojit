package autojit
import utest._
class Tape() {
  var tape = new Array[Int](1)
  var index = 0

  def write(value: Int) = tape(index) = value
  def read() = tape(index)

  def left() = {
    index -= 1
    if (index < 0){
      val newTape = new Array[Int](tape.length * 2)
      System.arraycopy(tape, 0, newTape, tape.length, tape.length)
      index += tape.length
      tape = newTape
    }
  }
  def right() = {
    index += 1
    if (index >= tape.length){
      val newTape = new Array[Int](tape.length * 2)
      System.arraycopy(tape, 0, newTape, 0, tape.length)
      tape = newTape
    }
  }
}
trait BFBasicBlock{
  def apply(tape: Tape, output: StringBuffer): Unit
}
case class Add() extends BFBasicBlock {
  def apply(tape: Tape, output: StringBuffer) = tape.write(tape.read() + 1)
}
case class Sub() extends BFBasicBlock {
  def apply(tape: Tape, output: StringBuffer) = tape.write(tape.read() - 1)
}
case class Right() extends BFBasicBlock {
  def apply(tape: Tape, output: StringBuffer) = tape.right()
}
case class Left() extends BFBasicBlock {
  def apply(tape: Tape, output: StringBuffer) = tape.left()
}
case class Print() extends BFBasicBlock {
  def apply(tape: Tape, output: StringBuffer) = output.append(tape.read().toChar)
}
case class Loop(children: Seq[BFBasicBlock])extends BFBasicBlock {
  def apply(tape: Tape, output: StringBuffer) = {
    while (tape.read() != 0) {
      children.foreach(_.apply(tape, output))
    }
  }
}
object BFTests extends TestSuite{

  def tests = Tests{
    val insns =
      "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>" +
      "---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

    val jumpTargets = Array.fill(insns.length)(-1)

    var stack = 0
    val stackMap = collection.mutable.Map.empty[Int, Int]
    for(i <- insns.indices){
      insns.charAt(i) match{
        case '[' =>
          stackMap(stack) = i
          stack += 1
        case ']' =>
          stack -= 1
          jumpTargets(i) = stackMap(stack)
          jumpTargets(stackMap(stack)) = i
        case _ =>
      }
    }

    val tape = new Tape()
    'direct - {

      var insnPtr = 0
      val output = new StringBuffer()
      while(insnPtr < insns.length){
        insns.charAt(insnPtr) match{
          case '>' => tape.right()
          case '<' => tape.left()
          case '+' => tape.write(tape.read() + 1)
          case '-' => tape.write(tape.read() - 1)
          case '.' => output.append(tape.read().toChar)
          case ',' => ??? // not implemented
          case '[' => if (tape.read() == 0) insnPtr = jumpTargets(insnPtr)
          case ']' => if (tape.read() != 0) insnPtr = jumpTargets(insnPtr)
        }
        insnPtr += 1
      }

      output.toString

    }
    'interpreted - {

      def recurse(insns: String, start: Int, end: Int): Seq[BFBasicBlock] = {
        val out = collection.mutable.Buffer.empty[BFBasicBlock]
        var i = start
        while (i < end){
          insns.charAt(i) match{
            case '>' => out.append(Right())
            case '<' => out.append(Left())
            case '+' => out.append(Add())
            case '-' => out.append(Sub())
            case '.' => out.append(Print())
            case ',' => ??? // not implemented
            case '[' =>
              out.append(Loop(recurse(insns, i+1, jumpTargets(i))))
              i = jumpTargets(i)
          }
          i += 1
        }
        out
      }

      val structured = recurse(insns, 0, insns.length)
      val output = new StringBuffer()
      structured.foreach(_.apply(tape, output))
      output.toString
    }
  }
}
