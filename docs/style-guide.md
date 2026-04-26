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
