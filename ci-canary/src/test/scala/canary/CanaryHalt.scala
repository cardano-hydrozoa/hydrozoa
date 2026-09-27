package canary

import org.scalatest.funsuite.AnyFunSuite

/** Stands in for a suite that hangs or whose JVM dies: with `-Dcanary.halt`, it halts the test JVM
  * mid-suite, so the suite never ends. Without it, it passes.
  */
class CanaryHalt extends AnyFunSuite {
    test("halts the test JVM when asked") {
        if sys.props.contains("canary.halt") then Runtime.getRuntime.halt(1)
    }
}
