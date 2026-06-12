package hydrozoa.lib.classification

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.syntax.all.*

// Observation: The semantics of this are very similar to cardano `Value`s
// TODO: in the future, we might want something more "bi-directional", such that we don't lose the actual values
//   that were classified. This could allow use to merge compatible historgrams, which may be important
//   for combining open petri net models. But for now, this is sufficient.

// ─── Error type ──────────────────────────────────────────────────────────────

/** Produced when a value matches more than one classifier function. [[Classifier.classifyAll]] is
  * always disjoint: every value must match at most one fn.
  */
final case class AmbiguousValue[Buckets, V](value: V, matchedBuckets: NonEmptyList[Buckets])

// TODO: Pretty printer
final case class Histogram[
    Bucket,
    V
] private[classification] (
    classified: Map[Bucket, Int],
    classifier: Classifier[Bucket, V]
) {

    override def toString: String =
        classified.toList.map((k, v) => (k.toString, v)).sorted.mkString("\n")

    /** Returns the count for `key`, or 0 if no value was classified into this bucket. */
    def apply(key: Bucket): Int = classified.getOrElse(key, 0)

    /** Total of all bucket counts
      *
      * @return
      */
    def size: Int = classified.values.sum

    def add(v: V): Either[AmbiguousValue[Bucket, V], Histogram[Bucket, V]] =
        classifier(v) match {
            case Left(e)    => Left(e)
            case Right(res) => Right(addHelper(res))
        }

    private def addHelper(b: Bucket): Histogram[Bucket, V] =
        val newMap = this.classified.updatedWith(b) {
            case None    => Some(1)
            case Some(n) => Some(n + 1)
        }
        this.copy(classified = newMap)

    def addAll(vs: Iterable[V]): ValidatedNel[AmbiguousValue[Bucket, V], Histogram[Bucket, V]] = {
        val res: (Histogram[Bucket, V], List[AmbiguousValue[Bucket, V]]) =
            vs.foldLeft((this, List.empty[AmbiguousValue[Bucket, V]])) { case ((hist, errors), v) =>
                classifier(v) match {
                    case Right(res) => (hist.addHelper(res), errors)
                    case Left(e)    => (hist, errors.prepended(e))
                }
            }
        if res._2.isEmpty then Valid(res._1) else Invalid(NonEmptyList.fromListUnsafe(res._2))
    }
}

object Histogram:
    def empty[Bucket, V](
        classifier: Classifier[Bucket, V],
    ): Histogram[Bucket, V] =
        new Histogram(Map.empty, classifier)

// ─── Classifier ──────────────────────────────────────────────────────────────

/** Classifies values of type `V` into buckets of type `K`.
  *
  * Subclass to provide [[fns]]. Classification is always disjoint: a value matching more than one
  * fn yields [[AmbiguousValue]]. Values matching no fn are assigned to the [[NoMatch]] bucket. Use
  * [[classify]] for single values and [[classifyAll]] for iterables.
  */
abstract case class Classifier[Bucket, V](defaultBucket: Bucket):

    def apply(v: V): Either[AmbiguousValue[Bucket, V], Bucket] = classify(v)

    /** Classifier functions. Each may assign a value to at most one bucket. */
    def classifierFns: List[V => Option[Bucket]]

    /** Classify a single value into a fresh [[Classification]]. */
    final def classify(v: V): Either[AmbiguousValue[Bucket, V], Bucket] = {
        val matches: List[Bucket] = classifierFns.flatMap(_(v))
        matches match {
            case Nil           => Right(defaultBucket)
            case bucket :: Nil => Right(bucket)
            case k             => Left(AmbiguousValue(v, NonEmptyList.fromListUnsafe(k)))
        }
    }
