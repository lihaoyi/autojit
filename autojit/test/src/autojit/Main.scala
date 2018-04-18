package autojit

import sbt.testing._

import scala.collection.mutable

object Main {
  def main(args: Array[String]): Unit = {
    val frameworks = Seq(classOf[utest.runner.Framework].newInstance())

    val events = mutable.Buffer.empty[Event]

    for(framework <- frameworks){
      val runner = framework.runner(args, args, getClass.getClassLoader)

      val testClasses = Seq(
        SimpleTests.getClass -> frameworks(0).fingerprints().head,
        ComputationTests.getClass -> frameworks(0).fingerprints().head,
        FailureTests.getClass -> frameworks(0).fingerprints().head
      )

      val tasks = runner.tasks(
        for ((cls, fingerprint) <- testClasses.toArray)
        yield new TaskDef(cls.getName.stripSuffix("$"), fingerprint, true, Array(new SuiteSelector))
      )

      val taskQueue = tasks.to[mutable.Queue]
      while (taskQueue.nonEmpty){
        val next = taskQueue.dequeue().execute(
          new EventHandler {
            def handle(event: Event) = events.append(event)
          },
          Array(
            new Logger {
              def debug(msg: String) = println(msg)

              def error(msg: String) = println(msg)

              def ansiCodesSupported() = true

              def warn(msg: String) = println(msg)

              def trace(t: Throwable) = t.printStackTrace(System.out)

              def info(msg: String) = println(msg)
            })
        )
        taskQueue.enqueue(next:_*)
      }
      println(runner.done())
    }
  }
}
