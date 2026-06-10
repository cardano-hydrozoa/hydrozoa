package hydrozoa.lib.classification

import cats.data.Validated
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Gen, Prop, Properties}

extension [K, V](classifier: Classifier[K, V])
    /** Returns a [[Properties]] that verifies the [[Histogram]] invariants for this classifier.
      *
      * Auto-derivable: bring a `Gen[V]` into scope and call `myClassifier.classifyAllProperties` to
      * get a runnable ScalaCheck suite.
      *
      * Invariants checked:
      *   1. Histogram size == input size: every value (including duplicates) contributes exactly 1
      *      to the total count (either in a classified bucket or [[ClassificationResult.NoMatch]]).
      */
    def classifyAllProperties(using gen: Gen[V]): Properties =
        new Properties(s"Classifier[${classifier.getClass.getSimpleName}]"):

            val _ = property("Classifier disjointness and size invariant check") =
                Prop.forAll(Gen.listOf(gen)) { (values: List[V]) =>
                    Histogram.empty(classifier).addAll(values) match
                        case Validated.Valid(h) =>
                            (h.size == values.size) :| "histogram size == input size"
                        case Validated.Invalid(errs) =>
                            false :| errs.toList.mkString(", ")
                }
