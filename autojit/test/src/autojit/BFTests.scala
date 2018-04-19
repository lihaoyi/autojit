package autojit
import utest._
class Tape() {
  var tape = new Array[Int](100)
  var index = 50

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
trait BFNode{
  def apply(tape: Tape, output: StringBuffer): Unit
}
case class Add() extends BFNode {
  def apply(tape: Tape, output: StringBuffer) = tape.write(tape.read() + 1)
}
case class Sub() extends BFNode {
  def apply(tape: Tape, output: StringBuffer) = tape.write(tape.read() - 1)
}
case class Right() extends BFNode {
  def apply(tape: Tape, output: StringBuffer) = tape.right()
}
case class Left() extends BFNode {
  def apply(tape: Tape, output: StringBuffer) = tape.left()
}
case class Print() extends BFNode {
  def apply(tape: Tape, output: StringBuffer) = {
    if (output != null) output.append(tape.read().toChar)
  }
}
case class Loop(children: BFNode) extends BFNode {
  def apply(tape: Tape, output: StringBuffer) = {
    while (tape.read() != 0) {
      children.apply(tape, output)
    }
  }
}
case class Block(items: IndexedSeq[BFNode]) extends BFNode with Intrinsics.Foreach[BFNode]{
  def apply(tape: Tape, output: StringBuffer) = items.foreach(_.apply(tape, output))
}
object BFTests extends TestSuite{
  def direct(insns: String, tape: Tape, output: StringBuffer, jumpTargets: Array[Int]) = {
    var insnPtr = 0
    while(insnPtr < insns.length){
      insns.charAt(insnPtr) match{
        case '>' => tape.right()
        case '<' => tape.left()
        case '+' => tape.write(tape.read() + 1)
        case '-' => tape.write(tape.read() - 1)
        case '.' => if (output != null) output.append(tape.read().toChar)
        case ',' => ??? // not implemented
        case '[' => if (tape.read() == 0) insnPtr = jumpTargets(insnPtr)
        case ']' => if (tape.read() != 0) insnPtr = jumpTargets(insnPtr)
      }
      insnPtr += 1
    }
  }
  def recurse(insns: String, start: Int, end: Int, jumpTargets: Array[Int]): Block = {
    val out = collection.mutable.ArrayBuffer.empty[BFNode]
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
          out.append(Loop(recurse(insns, i+1, jumpTargets(i), jumpTargets)))
          i = jumpTargets(i)
      }
      i += 1
    }
    Block(out)
  }

  def computeJumpTargets(insns: String) = {
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

    jumpTargets
  }
  def tests = Tests{
    val insns1 =
      "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>" +
      "---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

    val insns2 = "+++>+++++[-<+>]<."
    val jumpTargets1 = computeJumpTargets(insns1)
    val jumpTargets2 = computeJumpTargets(insns2)

    'simple - {
      'direct - {
        val tape = new Tape()
        val output = new StringBuffer()
        direct(insns2, tape, output, jumpTargets2)
        output.toString.map(_.toInt)
      }
      'parsed - {
        val structured = recurse(insns2, 0, insns2.length, jumpTargets2)
        val output = new StringBuffer()
        'interpreted - {
          val tape = new Tape()
          structured.apply(tape, output)
          output.toString.map(_.toInt)
        }
        'jitted - {
          val tape = new Tape()
          Lib.devirtualize[BFNode](structured, "apply").apply(tape, output)
          output.toString.map(_.toInt)
        }
      }

    }
    'hello - {
      'direct - {
        val tape = new Tape()

        val output = new StringBuffer()
        direct(insns1, tape, output, jumpTargets1)

        output.toString

      }
      'parsed - {

        val structured = recurse(insns1, 0, insns1.length, jumpTargets1)
        val output = new StringBuffer()
        'interpreted - {
          val tape = new Tape()
          structured.apply(tape, output)
          output.toString
        }
        'jitted - {
          val tape = new Tape()
          Lib.devirtualize[BFNode](structured, "apply").apply(tape, output)
          output.toString
        }
//        'perf - {
//          for(i <- 0 until 3)yield{
//            val start1 = System.currentTimeMillis()
//            var count1 = 0
//            while(System.currentTimeMillis() - start1 < 10000){
//              structured.apply(new Tape(), null)
//              structured.apply(new Tape(), null)
//              structured.apply(new Tape(), null)
//              structured.apply(new Tape(), null)
//              structured.apply(new Tape(), null)
//              count1 += 1
//            }
//
//            val jitted = Lib.devirtualize[BFNode](structured, "apply")
//            val start2 = System.currentTimeMillis()
//            var count2 = 0
//            while(System.currentTimeMillis() - start2 < 10000){
//              jitted.apply(new Tape(), null)
//              jitted.apply(new Tape(), null)
//              jitted.apply(new Tape(), null)
//              jitted.apply(new Tape(), null)
//              jitted.apply(new Tape(), null)
//              count2 += 1
//            }
//            val start3 = System.currentTimeMillis()
//            var count3 = 0
//            while(System.currentTimeMillis() - start3 < 10000){
//              direct(insns2, new Tape(), null, jumpTargets2)
//              direct(insns2, new Tape(), null, jumpTargets2)
//              direct(insns2, new Tape(), null, jumpTargets2)
//              direct(insns2, new Tape(), null, jumpTargets2)
//              direct(insns2, new Tape(), null, jumpTargets2)
//              count3 += 1
//            }
//            (count1, count2, count3)
//          }
//        }
      }
    }
  }
}
