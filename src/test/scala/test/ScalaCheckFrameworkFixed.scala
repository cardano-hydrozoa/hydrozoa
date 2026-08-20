package test

import java.util.concurrent.{Callable, ExecutorService, Executors}
import sbt.testing.*

/** ScalaCheck's sbt integration, with a suite's property events delivered as one batch.
  *
  * ⚠️ Without this, a falsified property is reported as `Passed: Total 1` with exit code 0. sbt's
  * forked worker keys a task's events on `taskDef.fullyQualifiedName()`, and ScalaCheck enumerates
  * one sub-task per property, all carrying the *same* name — so a suite's events arrive as several
  * batches under one key and only the last survives.
  *
  * ScalaCheck takes a single-task path when it recognises a forked run, which it detects by looking
  * for `ForkMain` in the fingerprint's class name. sbt 2 calls that class
  * `sbt.internal.worker1.ForkTestMain$SubclassFingerscan`, so the check misses. Filed as
  * typelevel/scalacheck#1195 and sbt/sbt#9642; remove this wrapper once either fix ships in a
  * version the build resolves.
  */
final class ScalaCheckFrameworkFixed extends Framework {
    private val underlying = new org.scalacheck.ScalaCheckFramework

    def name(): String = underlying.name()
    def fingerprints(): Array[Fingerprint] = underlying.fingerprints()

    def runner(args: Array[String], remoteArgs: Array[String], loader: ClassLoader): Runner = {
        val delegate = underlying.runner(args, remoteArgs, loader)
        new Runner {
            def args(): Array[String] = delegate.args()
            def remoteArgs(): Array[String] = delegate.remoteArgs()
            def done(): String = delegate.done()
            def tasks(taskDefs: Array[TaskDef]): Array[Task] =
                delegate.tasks(taskDefs).map(oneBatch)
        }
    }

    /** Run `root`'s sub-tasks to exhaustion against the caller's handler, so every event is
      * delivered from this one task. `root.execute` only *enumerates* the per-property tasks, so
      * running them here is the work sbt would otherwise have scheduled itself.
      */
    private def oneBatch(root: Task): Task = new Task {
        def taskDef(): TaskDef = root.taskDef()
        def tags(): Array[String] = root.tags()

        def execute(handler: EventHandler, loggers: Array[Logger]): Array[Task] = {
            var pending = root.execute(handler, loggers).toList
            while pending.nonEmpty do
                val futures = pending.map(task =>
                    ScalaCheckFrameworkFixed.properties.submit(new Callable[Array[Task]] {
                        def call(): Array[Task] = task.execute(handler, loggers)
                    })
                )
                pending = futures.flatMap(_.get().toList)
            Array.empty[Task]
        }
    }
}

object ScalaCheckFrameworkFixed {

    /** One pool for every suite in this JVM, sized to the box: 4 threads on a CI runner, more on a
      * workstation. It replaces sbt's own scheduling of the per-property tasks, so a suite's
      * properties now fan out together rather than queueing as separate sbt tasks. Fork-join
      * workers are daemons, so it needs no shutdown.
      */
    private lazy val properties: ExecutorService =
        Executors.newWorkStealingPool(math.max(2, Runtime.getRuntime.availableProcessors))
}
