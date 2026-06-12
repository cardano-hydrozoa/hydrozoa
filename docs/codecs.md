# Codecs Guide

> Note: I (Peter) at just jotting down notes here as I think of them. It can get refined into something more
> professional later.

# Conventions

- This uses the usual RFC style `MUST`, `SHOULD`, `MAY`, etc.
- But you should consider deviations from `MUST` as bugs and possibly grounds for witholding PR approval
- And deviations from `SHOULD` should be justified in doc comments, PR comments, etc.

# Rationale

- Our codecs are a bit messy right now. We have duplicate codecs spread across a few different places and no 
  standardized conventions. It's difficult to know where to find a specific codec, or if it exists.
- This is leading to duplicate codecs that are sometimes incompatible
- Some codecs are total; others are partial. Mixing conventions will eventually lead to runtime issues

# General Guidelines

- We have multiple codecs (json, cbor, pretty printing). When we have multiple in scope, you MUST qualify them _both_
   -- `borer.Codec` and `io.circe.Codec` -- rather than leaving one qualified and one un-qualified
- All codecs SHOULD live in the companion object of the relevant type. 
  - If we do not own the type (such as for scalus types), you SHOULD put the codecs in  
- Contextually-provided Codecs MUST use `given`s instead of `implicit val`
  - Codecs that are not contextually provided MUST contain a doc string explaining what the purpose of the codec is.
    Examples: Encoders used solely for logging that don't round-trip, or alternative codecs for specific integrations
    (i.e., deviations from CIP-0116) for sugar-rush compatability. 
- given codecs that require some additional context MUST declare it in a `using` clause, such as
  `given (config : CardanoNetwork.Secion) : Codec[Foo]`
- Codecs for sealed traits MUST re-export the codecs for the implementations of the trait in the companion object of
  the trait
- All `given` codecs MUST have round-trip and golden tests
  - (This isn't something we do yet, but we really should)
- A public `given` codec in the companion object signifies that this is the "default" codec. If the codec is not 
  default, simply use a `def` instead.
- If you have a non-default codec, and you want to use it as a `given` to avoid `using` clauses, make it `private`.
- Prefer `Codec` to separate `Encoder` and `Decoder` implementations, where possible. This makes imports less verbose.
  - The primary exception to this is when the encoder loses ambient context that must be restored in the decoder
    (like `using CardanoNetwork.Section`)
- If there are a collection of related codecs fit for a specific purpose, `Foo`, (such as the hydrozoa server codecs vs the
  remote l2 ledger codecs), you SHOULD re-export them in a `FooCodecs` object.
- You MUST obey the totality conventions of the library in use.
  - If the codec's `decode` method is partial (i.e., throws an exception), then implementations of the codec MUST be 
    partial
  - If the codec's `decode` method is total (i.e., returns `Either`), then implementations must be total.
  - Explicitly: 
    - borer's CBOR codecs are partial. This means we should wrap in a `Try` at the _call site_, and MUST NOT
      return `Either`.
    - circe's JSON codecs are total. This means that if we are wrapping a partial function (such as `ByteString.fromHex` 
      or a wrapped cbor decoding), we MUST catch possible exceptions (i.e., with `Try`) at the _definition site_ of the
      codec

