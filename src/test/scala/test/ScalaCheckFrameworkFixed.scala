package test

import java.util.concurrent.{Callable, ExecutorService, Executors}
import sbt.testing.*

/** ScalaCheck's sbt integration, with a suite's property events delivered as one batch.
  *
  * sbt's forked test worker collects a task's events and reports them keyed on
  * `taskDef.fullyQualifiedName()`. ScalaCheck (when it does not recognise a forked run) enumerates
  * one sub-task per property, and all of them carry the *same* fully-qualified name, differing only
  * by their `TestSelector` — so a suite's events arrive as several batches under one key and only
  * one survives. A falsified property is then reported as `Passed: Total 1` with exit code 0.
  *
  * That is typelevel/scalacheck#185. ScalaCheck fixed it in #388 by taking a single-task path when
  * it detects a forked run, which it does by looking for the string `ForkMain` in the fingerprint's
  * class name. sbt 2 renamed that class to `sbt.internal.worker1.ForkTestMain$SubclassFingerscan`,
  * so the check no longer matches and the 2015 behaviour is back. Filed as
  * typelevel/scalacheck#1195; the sbt half — a group's events overwriting one another rather than
  * accumulating — as sbt/sbt#9642.
  *
  * This wrapper does not change how properties run — the sub-tasks are still executed concurrently
  * — only where their events are delivered from. Remove it once either fix ships in a version this
  * build resolves.
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
      * running them here is the same work sbt would otherwise have scheduled itself — and against
      * the same handler, which collects into a concurrent queue.
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

    /** Shared across every suite in this JVM and bounded, so replacing sbt's scheduling of the
      * per-property tasks does not multiply the concurrency the build already caps (`Global /
      * concurrentRestrictions`). Fork-join workers are daemons, so it needs no shutdown.
      */
    private lazy val properties: ExecutorService =
        Executors.newWorkStealingPool(math.max(2, Runtime.getRuntime.availableProcessors))
}
