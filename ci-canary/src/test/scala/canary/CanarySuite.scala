package canary

import org.scalatest.funsuite.AnyFunSuite

/** Tests with known outcomes, which CI's summary must report exactly; see `just ci-canary`. */
class CanarySuite extends AnyFunSuite {
    test("passes")(assert(1 + 1 == 2))
    test("fails, by design: with a comma and a colon")(assert(1 + 1 == 3, "the canary's failure"))
    test("is cancelled")(assume(false, "the canary's cancellation"))
    ignore("is ignored")(assert(true))
    test("is pending")(pending)
}
