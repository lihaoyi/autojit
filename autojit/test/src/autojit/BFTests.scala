package autojit
import utest._
object BFTests extends TestSuite{
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

  def tests = Tests{
    'hello - {
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
  }
}
