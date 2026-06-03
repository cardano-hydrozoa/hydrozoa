# Hydrozoa Scala Style Guide

## Opaque type conversions to tuples

### Rule

When an opaque type wraps a tuple whose elements are themselves opaque types, the
`Conversion` given **must** call `.convert` explicitly on each element. Using `identity`
or an implicit coercion in the lambda body causes either a stack overflow or an
infinite loop.

### Why it breaks

**Pattern 1 — `identity` body:**

```scala
opaque type Id = (HeadPeerNumber, RequestNumber)
given Conversion[Id, (Int, Long)] = identity  // BROKEN
```

1. Compiler elaborates `identity[Id]`, type `Id => Id`.
2. Return type `Id` must coerce to `(Int, Long)`.
3. Searches for `Conversion[Id, (Int, Long)]` — finds the given currently being
   elaborated (Scala 3 puts a given in scope during its own body elaboration).
4. Recurse to step 1 → stack overflow.

**Pattern 2 — implicit coercion in lambda body:**

```scala
given Conversion[Id, (Int, Long)] = id => id._1 -> id._2  // BROKEN
```

1. `id._1` gives `HeadPeerNumber`, which must coerce to `Int`.
2. Two candidates: `Conversion[HeadPeerNumber, Int]` (remote) and
   `Conversion[Id, (Int, Long)]` (local). **Local scope wins.**
3. Compiler rewrites `id._1: Int` as `given_Conversion.apply(id)._1` —
   applies the whole conversion to `id` first, then projects `._1`.
4. That calls the lambda again with the same `id` → infinite loop.

### Fix

Always call `.convert` explicitly on each element so the compiler resolves to the
element's own companion-scope conversion, not the enclosing one:

```scala
given Conversion[Id, (Int, Long)] = id => id._1.convert -> id._2.convert  // CORRECT
```

See `RequestId.scala` for the canonical working example.

### Safe exception

`identity` is safe when the opaque type wraps a tuple of **primitive** (non-opaque)
types, because no element-level conversion search is needed:

```scala
opaque type Full = (Int, Int)
given Conversion[Full, (Int, Int)] = identity  // safe — underlying IS (Int, Int)
```

## Boolean predicates — `is*` / `has*` / `can*` naming

### Rule

A `def` or `val` that returns `Boolean` is named as a predicate that reads like a
yes/no question — prefix it with `is`, `has`, `can` (or another boolean-flavoured
verb) so call sites read as assertions:

```scala
private def isSaturated(present: Set[PeerId]): Boolean = ...
if isSaturated(cell.round1.keySet) then completeRound1(...)   // "if … is saturated"
```

Not:

```scala
private def saturated(present: Set[PeerId]): Boolean = ...
if saturated(cell.round1.keySet) then ...                     // reads as a noun, hides the test
```

### Why

`if isSaturated(x)` reads as a sentence; `if saturated(x)` reads as a noun and
hides that it is a test. The prefix also disambiguates a boolean check from a
value of the same root name (`val saturated: Set[…]` vs `def isSaturated:
Boolean`). `HeadPeerId.isLeader` / `isSlowLeader` are the in-repo examples.

### Scope

Pick the prefix that fits: `is*` for a state (`isSaturated`, `isLeader`), `has*`
for presence (`hasQuorum`), `can*` for capability (`canSubmit`). Don't force `is`
onto established collection-style predicates that already read as a test
(`contains`, `exists`, `startsWith`).
