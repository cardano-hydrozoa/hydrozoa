package org.scalacheck

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import org.scalacheck.Prop.{False, True}
import org.scalacheck.rng.Seed
import org.scalacheck.util.Pretty
import scala.util.{Failure, Success, Try}

sealed trait PropertyEffect[+A] {
    def run(using IORuntime): A
}

case class IOEffect[A](io: IO[A]) extends PropertyEffect[A] {
    def run(using runtime: IORuntime): A = io.unsafeRunSync()
}

case class PureEffect[A](value: A) extends PropertyEffect[A] {
    def run(using IORuntime): A = value
}

case class TryEffect[A](tryValue: Try[A]) extends PropertyEffect[A] {
    def run(using IORuntime): A = tryValue.get
}

/** Represents a single step in building a property
  */
sealed trait PropertyStep
case class PickStep[A](gen: Gen[A], label: String, pretty: A => Pretty) extends PropertyStep
case class RunStep[A](effect: PropertyEffect[A]) extends PropertyStep
case class AssertStep(passed: Boolean, msg: String, location: String) extends PropertyStep
case class PreStep(satisfied: Boolean) extends PropertyStep
case class MonitorStep(f: Prop => Prop) extends PropertyStep

/** Immutable record of property execution
  */
case class PropertyTrace(
    steps: List[PropertyStep] = Nil,
    generatedValues: List[(String, Any)] = Nil,
    stopped: Option[Prop] = None
) {
    def addStep(step: PropertyStep): PropertyTrace =
        copy(steps = steps :+ step)

    def addGenerated(label: String, value: Any): PropertyTrace =
        copy(generatedValues = generatedValues :+ (label -> value))

    def withStopped(prop: Prop): PropertyTrace =
        copy(stopped = Some(prop))
}

class PropertyBuilder[M[_]] private (
    private val seed: rng.Seed,
    private val params: Gen.Parameters,
    private var trace: PropertyTrace = PropertyTrace()
)(using runtime: IORuntime) {

    private var currentSeed = seed

    /** Generate a value with automatic location tracking */
    def pick[A](gen: Gen[A], label: String = "")(using pp: A => Pretty): A = {
        requireNotStopped("pick")

        val location = getCallerLocation()
        val step = PickStep(gen, if label.isEmpty then s"Gen@$location" else label, pp)
        trace = trace.addStep(step)

        val result = gen.doApply(params, currentSeed)
        currentSeed = result.seed

        result.retrieve match {
            case Some(value) =>
                trace = trace.addGenerated(step.label, value)
                value
            case None =>
                trace = trace.withStopped(Prop.undecided)
                throw StoppedException(Prop.undecided)
        }
    }

    /** Run an effect - supports IO, Try, pure values */
    def run[A](effect: PropertyEffect[A]): A = {
        requireNotStopped("run")

        trace = trace.addStep(RunStep(effect))

        Try(effect.run) match {
            case Success(value) => value
            case Failure(e) =>
                trace = trace.withStopped(Prop.exception(e))
                throw StoppedException(Prop.exception(e))
        }
    }

    /** Convenience: run IO directly */
    def runIO[A](io: IO[A]): A = run(IOEffect(io))

    /** Convenience: wrap code in Try and run it - handles exceptions gracefully */
    def runTry[A](block: => A): A = run(TryEffect(Try(block)))

    /** Make an assertion with automatic location tracking */
    def assert(condition: => Boolean, msg: String = ""): Unit = {
        requireNotStopped("assert")

        val location = getCallerLocation()
        val finalMsg = if msg.isEmpty then s"Assertion failed at $location" else msg

        val passed = condition // Evaluate immediately
        trace = trace.addStep(AssertStep(passed, finalMsg, location))

        if !passed then {
            val prop = buildFailedProp(finalMsg)
            trace = trace.withStopped(prop)
            throw StoppedException(prop)
        }
    }

    /** Precondition - discards test if false */
    def pre(condition: => Boolean): Unit = {
        requireNotStopped("pre")

        val satisfied = condition // Evaluate immediately
        trace = trace.addStep(PreStep(satisfied))

        if !satisfied then {
            trace = trace.withStopped(Prop.undecided)
            throw StoppedException(Prop.undecided)
        }
    }

    /** Stop with custom property */
    def stop(prop: Prop): Nothing = {
        trace = trace.withStopped(prop)
        throw StoppedException(prop)
    }

    /** Add monitoring function */
    def monitor(f: Prop => Prop): Unit = {
        trace = trace.addStep(MonitorStep(f))
    }

    /** Collect statistics */
    def collect[A](value: A): Unit = {
        monitor(Prop.collect(value))
    }

    /** Label the property */
    def label(msg: String): Unit = {
        monitor(msg |: _)
    }

    // ===================================
    // Internal helpers
    // ===================================

    private def requireNotStopped(operation: String): Unit = {
        if trace.stopped.isDefined then {
            throw new IllegalStateException(
              s"Cannot call $operation after property has stopped"
            )
        }
    }

    private def getCallerLocation(): String = {
        val trace = Thread.currentThread().getStackTrace
        // Skip getStackTrace, getCallerLocation, and the calling method
        val caller = trace.drop(3).headOption
        caller.map(e => s"${e.getFileName}:${e.getLineNumber}").getOrElse("unknown")
    }

    private def buildFailedProp(msg: String): Prop = {
        val args = trace.generatedValues.map { case (label, value) =>
            Prop.Arg(label, value, 0, value, value.toString, value.toString)
        }

        var prop: Prop = Prop(false)

        // Apply all monitors
        trace.steps.collect { case MonitorStep(f) => f }.foreach { f =>
            prop = f(prop)
        }

        val result = Prop.Result(
          status = False,
          args = args,
          collected = Set.empty,
          labels = Set(msg)
        )

        Prop { _ => result }
    }

    private def buildSuccessProp(): Prop = {
        val args = trace.generatedValues.map { case (label, value) =>
            Prop.Arg(label, value, 0, value, value.toString, value.toString)
        }

        var prop: Prop = Prop(true)

        // Apply all monitors
        trace.steps.collect { case MonitorStep(f) => f }.foreach { f =>
            prop = f(prop)
        }

        val result = Prop.Result(
          status = True,
          args = args,
          collected = Set.empty,
          labels = Set.empty
        )

        Prop { _ => result }
    }

    /** Execute property block and build result */
    private[scalacheck] def execute(block: => Boolean): Prop = {
        try {
            val result = block

            trace.stopped match {
                case Some(prop) => prop
                case None =>
                    if result then buildSuccessProp()
                    else buildFailedProp("Property returned false")
            }
        } catch {
            case StoppedException(prop) => prop
            case e: Throwable           => Prop.exception(e)
        }
    }

    /** Get execution trace for debugging */
    def getTrace: PropertyTrace = trace
}

/** Exception used internally to short-circuit execution */
case class StoppedException(prop: Prop) extends Exception("Property stopped", null, false, false)

object PropertyBuilder {

    /** Create a property from imperative block */
    def property(block: PropertyBuilder[IO] => Boolean)(using runtime: IORuntime): Prop = {
        Prop { params =>
            val (p, seed) = Prop.startSeed(params)
            val builder = new PropertyBuilder[IO](seed, p)

            val result = builder.execute(block(builder))
            result(params)
        }
    }

    /** Create property with custom seed (for deterministic testing) */
    def propertyWithSeed(seed: rng.Seed)(
        block: PropertyBuilder[IO] => Boolean
    )(using runtime: IORuntime): Prop = {
        Prop { params =>
            val builder = new PropertyBuilder[IO](seed, params)
            val result = builder.execute(block(builder))
            result(params)
        }
    }

    /** Combine multiple properties */
    def all(props: Prop*): Prop = {
        Prop.all(props*)
    }

    /** Either property passes */
    def any(props: Prop*): Prop = {
        Prop.atLeastOne(props*)
    }
}

// ===================================
// Enhanced Usage Examples
// ===================================

object PropertyBuilderTest extends Properties("PropertyBuilderTest") {

    // Need implicit IORuntime for IO effects
    import cats.effect.unsafe.implicits.global

    // Example 1: Dependent generation with labels
    property("list generation") = PropertyBuilder.property { p =>
        val size = p.pick(Gen.choose(1, 10), "size")
        p.collect(s"size=$size")

        val list = p.pick(Gen.listOfN(size, Arbitrary.arbitrary[Int]), "list")

        p.assert(list.length == size, s"Expected $size elements")
        true
    }

    // Example 2: Error handling
    property("handles failures gracefully") = PropertyBuilder.property { p =>
        val n = p.pick(Gen.choose(0, 10))

        p.pre(n > 0) // Skip if n is 0

        val result = p.run(TryEffect(Try(100 / n)))

        p.assert(result > 0)
        true
    }

    // Example 3: Debugging with trace
    property("trace example") = PropertyBuilder
        // .propertyWithSeed(Seed.fromBase64("TLR5sUXoUagIGs_9G_A6QLTLVqwAb5RJrH_EeGwc1eM=").get) {
        .property { p =>
            val x = p.pick(Gen.choose(1, 100), "x")
            val y = p.pick(Gen.choose(1, 100), "y")

            // On failure, you can inspect p.getTrace to see what was generated
            p.assert(x + y > 0)
            true
        }

    // Example 4: Using labels and collect
    property("statistics collection") = PropertyBuilder.property { p =>
        val category = p.pick(Gen.oneOf("small", "medium", "large"), "category")
        p.collect(category)
        p.label(s"Testing $category category")

        val value = category match {
            case "small"  => p.pick(Gen.choose(1, 10))
            case "medium" => p.pick(Gen.choose(11, 100))
            case "large"  => p.pick(Gen.choose(101, 1000))
        }

        p.assert(value > 0)
        true
    }

    // Example 5: Multiple assertions with good error messages
    property("multiple checks") = PropertyBuilder.property { p =>
        val numbers = p.pick(Gen.listOfN(5, Gen.choose(1, 100)), "numbers")

        p.assert(numbers.nonEmpty, "List should not be empty")
        p.assert(numbers.length == 5, s"Expected 5 elements, got ${numbers.length}")
        p.assert(numbers.forall(_ > 0), "All numbers should be positive")

        true
    }
}
