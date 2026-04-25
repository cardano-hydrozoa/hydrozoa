package hydrozoa.lib.cardano.scalus.gens

import _root_.scalus.cardano.ledger.{
    KeepRaw,
    Sized,
    TaggedOrderedSet,
    TaggedOrderedStrictSet,
    TaggedSortedMap,
    TaggedSortedSet,
    TaggedSortedStrictMap
}
import io.bullet.borer.Encoder
import izumi.reflect.Tag
import org.scalacheck.Gen
import registry.{Entry, TypedEntry}

import scala.collection.immutable.SortedMap

/** Container combinators that complement `registry.scalacheck.Containers` with shapes specific
  * to scalus's data model — `SortedMap[K, V]` and the three `Tagged*Set[A]` opaque types.
  *
  * All four follow the `mapOf[K, V]` / `setOf[T]` pattern: a `TypedEntry` whose inputs are
  * the per-element generators and whose closure threads them through the appropriate
  * `Container.from(...)` constructor.
  */
object Containers:

    /** Register `(Gen[K], Gen[V]) => Gen[SortedMap[K, V]]`. The implicit `Ordering[K]` is captured
      * once when the entry is built and reused inside the closure.
      *
      * Mirrors the shape of `registry.scalacheck.mapOf[K, V]`.
      */
    def sortedMapOf[K: Ordering, V](using
        kTag: Tag[Gen[K]],
        vTag: Tag[Gen[V]],
        outTag: Tag[Gen[SortedMap[K, V]]]
    ): TypedEntry[Gen[K] *: Gen[V] *: EmptyTuple, Gen[SortedMap[K, V]]] =
        val ord = summon[Ordering[K]]
        TypedEntry(
          Entry(
            inputs = List(kTag.tag, vTag.tag),
            output = outTag.tag,
            invoke = args =>
                val gK = args(0).asInstanceOf[Gen[K]]
                val gV = args(1).asInstanceOf[Gen[V]]
                Gen.mapOf(gK.flatMap(k => gV.map(v => (k, v)))).map(SortedMap.from(_)(using ord))
          )
        )

    /** Register `Gen[A] => Gen[TaggedSortedSet[A]]`. Requires `Ordering[A]` because
      * `TaggedSortedSet.from` is a `SortedSet.from`.
      */
    def taggedSortedSetOf[A: Ordering](using
        inTag: Tag[Gen[A]],
        outTag: Tag[Gen[TaggedSortedSet[A]]]
    ): TypedEntry[Gen[A] *: EmptyTuple, Gen[TaggedSortedSet[A]]] =
        val ord = summon[Ordering[A]]
        TypedEntry(
          Entry(
            inputs = List(inTag.tag),
            output = outTag.tag,
            invoke = args =>
                val gA = args(0).asInstanceOf[Gen[A]]
                Gen.listOf(gA).map(xs => TaggedSortedSet.from(xs)(using ord))
          )
        )

    /** Register `Gen[A] => Gen[TaggedOrderedSet[A]]`. */
    def taggedOrderedSetOf[A](using
        inTag: Tag[Gen[A]],
        outTag: Tag[Gen[TaggedOrderedSet[A]]]
    ): TypedEntry[Gen[A] *: EmptyTuple, Gen[TaggedOrderedSet[A]]] =
        TypedEntry(
          Entry(
            inputs = List(inTag.tag),
            output = outTag.tag,
            invoke = args =>
                val gA = args(0).asInstanceOf[Gen[A]]
                Gen.listOf(gA).map(TaggedOrderedSet.from)
          )
        )

    /** Register `Gen[A] => Gen[TaggedOrderedStrictSet[A]]`. The `from` constructor takes an
      * implicit `ProtocolVersion` parameter with a default value (Conway PV) — we let that
      * default kick in.
      */
    def taggedOrderedStrictSetOf[A](using
        inTag: Tag[Gen[A]],
        outTag: Tag[Gen[TaggedOrderedStrictSet[A]]]
    ): TypedEntry[Gen[A] *: EmptyTuple, Gen[TaggedOrderedStrictSet[A]]] =
        TypedEntry(
          Entry(
            inputs = List(inTag.tag),
            output = outTag.tag,
            invoke = args =>
                val gA = args(0).asInstanceOf[Gen[A]]
                Gen.listOf(gA).map(xs => TaggedOrderedStrictSet.from(xs))
          )
        )

    /** Register `Gen[A] => Gen[Sized[A]]`. Captures the implicit borer `Encoder[A]` once when
      * the entry is built — `Sized.apply[A: Encoder](value: A)` re-encodes the value to compute
      * its CBOR-byte length and stores it alongside the value.
      */
    def sized[A](using
        enc: Encoder[A],
        inTag: Tag[Gen[A]],
        outTag: Tag[Gen[Sized[A]]]
    ): TypedEntry[Gen[A] *: EmptyTuple, Gen[Sized[A]]] =
        TypedEntry(
          Entry(
            inputs = List(inTag.tag),
            output = outTag.tag,
            invoke = args =>
                val gA = args(0).asInstanceOf[Gen[A]]
                gA.map(a => Sized(a)(using enc))
          )
        )

    /** Register `Gen[A] => Gen[KeepRaw[A]]`. Captures the implicit borer `Encoder[A]` once at
      * entry-construction time — `KeepRaw.apply[A: Encoder](value: A)` runs the encoder lazily
      * (the raw bytes are computed on first access) but the encoder itself must be in scope.
      */
    def keepRaw[A](using
        enc: Encoder[A],
        inTag: Tag[Gen[A]],
        outTag: Tag[Gen[KeepRaw[A]]]
    ): TypedEntry[Gen[A] *: EmptyTuple, Gen[KeepRaw[A]]] =
        TypedEntry(
          Entry(
            inputs = List(inTag.tag),
            output = outTag.tag,
            invoke = args =>
                val gA = args(0).asInstanceOf[Gen[A]]
                gA.map(a => KeepRaw(a)(using enc))
          )
        )

    /** Register `Gen[A] => Gen[TaggedSortedMap[K, A]]`. The map's keys are derived from the values
      * via the implicit `TaggedSortedMap.KeyOf[K, A]` typeclass; ordering of keys is by the
      * implicit `Ordering[K]`. Both implicits are captured once at entry-construction time.
      */
    def taggedSortedMapOf[K: Ordering, A](using
        keyOf: TaggedSortedMap.KeyOf[K, A],
        inTag: Tag[Gen[A]],
        outTag: Tag[Gen[TaggedSortedMap[K, A]]]
    ): TypedEntry[Gen[A] *: EmptyTuple, Gen[TaggedSortedMap[K, A]]] =
        TypedEntry(
          Entry(
            inputs = List(inTag.tag),
            output = outTag.tag,
            invoke = args =>
                val gA = args(0).asInstanceOf[Gen[A]]
                Gen.listOf(gA).map(xs => TaggedSortedMap.from(xs))
          )
        )

    /** Register `Gen[A] => Gen[TaggedSortedStrictMap[K, A]]`. Same keying scheme as
      * `taggedSortedMapOf`; relies on the default `ProtocolVersion.conwayPV` for the
      * deduplication-vs-throw decision (in Conway, duplicate keys cause `from` to throw — given
      * that each generated `A` typically produces a distinct key in practice for the registered
      * usages, that's fine).
      */
    def taggedSortedStrictMapOf[K: Ordering, A](using
        keyOf: TaggedSortedStrictMap.KeyOf[K, A],
        inTag: Tag[Gen[A]],
        outTag: Tag[Gen[TaggedSortedStrictMap[K, A]]]
    ): TypedEntry[Gen[A] *: EmptyTuple, Gen[TaggedSortedStrictMap[K, A]]] =
        TypedEntry(
          Entry(
            inputs = List(inTag.tag),
            output = outTag.tag,
            invoke = args =>
                val gA = args(0).asInstanceOf[Gen[A]]
                Gen.listOf(gA).map(xs => TaggedSortedStrictMap.from(xs))
          )
        )
