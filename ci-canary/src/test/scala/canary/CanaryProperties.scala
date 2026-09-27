package canary

import org.scalacheck.{Prop, Properties}

/** Properties with known outcomes, which CI's summary must report exactly; see `just ci-canary`. */
object CanaryProperties extends Properties("CanaryProperties") {
    val _ = property("holds") = Prop.forAll((n: Int) => n + 0 == n)
    val _ = property("is falsified, by design") = Prop.forAll((n: Int) => n < n)
}
