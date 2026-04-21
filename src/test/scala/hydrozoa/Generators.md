# Generators

## Arbitrary generators

Generate completely random data, that can (or even SHOULD) cause errors, the name convention is:

```scala
arbitraryNetworkId: Gen[Byte]
```

## Purposed generators

### Simple generators

```scala
generateNetworkId: Gen[Byte]
```

```scala
arbitraryNetworkId: Gen[Byte]
```

Generates something respecting some bounds, the name convention is:

```scala
def generateFoo: Gen[Foo]
```


### Mandatory bounds

Should be represented as curried parameters:

```scala
generateFoo(bar: Bar): Gen[Foo]
```

Such generators cannot work as arbitrary ones, since they need some mandatory context.

### Optional bounds

Should be passed as uncurried parameters of type `... => Gen[Foo]` with mandatory defaults,
I think it's easier than using `Option`, though I didn't fully convince myself yet:

```scala

def generateFoo(
    generateBar: Gen[Bar] = arbitraryBar,
    generateBaz: Quux => Gen[Baz] = _ => arbitraryBaz
): Gen[Foo]

```

### Combined bounds

Mandatory and optional bounds can be combined:

```scala
def generateFoo(bar: Bar)(generateBaz: Geb[Baz] = arbitraryBaz): Gen[Foo]
```

### Purposed as Arbitrary

When not having mandatory bounds, a purposed generator should have an arbitrary alias:

```scala

def generateFoo(generateBar: Gen[Bar] = arbitraryBar): Gen[Foo]

def arbitraryFoo = generateFoo()
```

The bounds can be exogenous - here it's done by knowing upfront test peers:

```scala
def generateTestPeers(minPeers: Int = 2, maxPeers: Int = 5): Gen[TestPeers]
```

## Context-Dependent Generators

### The Problem

Many of our generators need contextual information to work properly. For example:
- **TestPeers**: Number of peers, their addresses, verification keys, wallets
- **CardanoNetwork**: Network parameters, slot configuration, protocol parameters
- **SlotConfig**: Needed for time-based calculations

**The Key Design Question**: When a generator is composed of many sub-generators, and you want to customize just one small part deep inside, how should the API work?

Two fundamentally different approaches emerged:
1. **Bubbling Up (Old)**: All customizable parts exposed as parameters at the top level
2. **Manual Composition (New)**: Caller builds the entire generator tree manually

### Old Approach: Bubbling Up (Before Peter's Refactor)

**Philosophy**: Every customizable part is "bubbled up" as a parameter to the top-level generator. You can customize any deep part without understanding the internal composition structure.

Generators took TestPeers as an explicit curried parameter and CardanoNetwork via function parameters:

```scala
// Type aliases for context-dependent generators
type GenesisUtxosGen = CardanoNetwork => Gen[Map[HeadPeerNumber, Utxos]]
type FallbackContingencyGen = CardanoNetwork => Gen[FallbackContingency]
type BlockCreationEndTimeGen = SlotConfig => Gen[BlockCreationEndTime]

// Generator signature
def generateInitializationParameters(testPeers: TestPeers)(
    generateFallbackContingency: FallbackContingencyGen = generateFallbackContingency,
    generateGenesisUtxosL1: GenesisUtxosGen,
    equityRange: (Coin, Coin) = (Coin(5_000_000), Coin(500_000_000))
): Gen[InitializationParameters] = {
    for {
        cardanoNetwork <- Gen.const(testPeers.network)
        fallbackContingency <- generateFallbackContingency(cardanoNetwork)
        genesisUtxos <- generateGenesisUtxosL1(cardanoNetwork)
        // ... rest of the generator
    } yield initParams
}
```

**Calling the generator (the key advantage - notice how simple this is!):**

```scala
override def genInitialState(env: Env): Gen[State] = {
    import env.testPeers
    val testPeerToUtxos = env.genesisUtxo(testPeers)

    // Just define the ONE part you want to customize
    val generateHeadStartTime: BlockCreationEndTimeGen = slotConfig =>
        Gen.const(BlockCreationEndTime(env.startTime.quantize(slotConfig)))

    for {
        // Plug it in at the top level - don't need to know WHERE it's used internally!
        config <- MultiNodeConfig.generateForTestPeers(testPeers)(
          generateBlockCreationEndTime = generateHeadStartTime,  // ← Just plug it in!
          generateTxTiming = generateDefaultTxTiming,
          generateInitializationParameters = InitializationParametersGenTopDown.GenWithDeps(
            generateGenesisUtxosL1 =
                network => Gen.const(testPeerToUtxos.map((k, v) => k.headPeerNumber -> v))
          )
        )
        // ... rest of the for-comprehension
    } yield state
}
```

**Why this is great:** Notice you don't need to know that `generateBlockCreationEndTime` is used inside `generateInitialBlock` which is inside `generateHeadConfig` - it's all bubbled up for you!

**Pros:**
- ✅ **Information hiding** - Don't need to understand internal generator composition
- ✅ **Plug and play** - Just provide the part you want to customize at the top level
- ✅ Simple and direct - TestPeers is explicitly passed
- ✅ Clear what context is needed - you see `testPeers` at the call site
- ✅ Concise at the call site - just pass a lambda for context-dependent sub-generators

**Cons:**
- ❌ Parameter explosion - Top-level generator has many parameters for all customizable parts
- ❌ Boilerplate in generator implementation - Every generator needs to manually extract `cardanoNetwork` from `testPeers`
- ❌ Inconsistent - Some context via curried params, some via function types
- ❌ Not composable - Can't easily chain generators without manually threading TestPeers

### New Approach: ReaderT Monad (Peter's Refactor)

Generators now use `GenWithTestPeers[A]` which is a type alias for `ReaderT[Gen, TestPeers, A]`. This threads the TestPeers context automatically through all generators.

```scala
// Type alias
type GenWithTestPeers[A] = ReaderT[Gen, TestPeers, A]

// Generator signature - NO MORE CURRIED TestPeers PARAMETER
def generateInitializationParameters(
    generateFallbackContingency: GenWithTestPeers[FallbackContingency] = generateFallbackContingency,
    generateGenesisUtxosL1: GenWithTestPeers[Map[HeadPeerNumber, Utxos]],
    equityRange: (Coin, Coin) = (Coin(5_000_000), Coin(500_000_000))
): GenWithTestPeers[InitializationParameters] = {
    for {
        testPeers <- ReaderT.ask[Gen, TestPeers]  // Get TestPeers from context
        cardanoNetwork = testPeers.network
        fallbackContingency <- generateFallbackContingency
        genesisUtxos <- generateGenesisUtxosL1
        // ... rest of the generator
    } yield initParams
}
```

**Calling the generator:**

```scala
override def genInitialState(env: Env): Gen[State] = {
    import env.testPeers
    val testPeerToUtxos = env.genesisUtxo(testPeers)

    // Build GenWithTestPeers generators - more verbose!
    val generateHeadStartTime: GenWithTestPeers[BlockCreationEndTime] =
        ReaderT(tp => Gen.const(BlockCreationEndTime(env.startTime.quantize(tp.slotConfig))))

    val generateHeadConfigBootstrap: GenWithTestPeers[HeadConfig.Bootstrap] =
        hydrozoa.config.head.generateHeadConfigBootstrap(
          generateInitializationParameters = InitParamsType.TopDown(
            InitializationParametersGenTopDown.GenWithDeps(
              generateGenesisUtxosL1 = ReaderT((tp: TestPeers) =>
                  Gen.const(testPeerToUtxos.map((k, v) => k.headPeerNumber -> v))
              )
            )
          )
        )

    val generateHeadConfig: GenWithTestPeers[HeadConfig] =
        hydrozoa.config.head.generateHeadConfig(
          genHeadConfigBootstrap = generateHeadConfigBootstrap,
          generateInitialBlock = bootstrap =>
              hydrozoa.config.head.initialization.generateInitialBlock(
                genHeadConfigBootstrap = ReaderT.pure[Gen, TestPeers, HeadConfig.Bootstrap](bootstrap),
                generateBlockCreationEndTime = generateHeadStartTime
              )
        )

    for {
        config <- MultiNodeConfig.generateForTestPeers(
          generateHeadConfig = generateHeadConfig
        ).run(testPeers)  // Extract the Gen by running the ReaderT with testPeers
        // ... rest of the for-comprehension
    } yield state
}
```

**Pros:**
- ✅ Consistent context handling - TestPeers automatically available via `ReaderT.ask`
- ✅ More composable - Generators compose naturally via monadic operations
- ✅ Less boilerplate in generator implementation - Don't need to manually thread TestPeers

**Cons:**
- ❌ **Much more verbose at call sites** - Must wrap everything in `ReaderT(...)`
- ❌ **Less intuitive** - ReaderT monad is advanced Scala, harder for newcomers
- ❌ **Requires explicit `.run(testPeers)`** - Have to "run" the ReaderT to get Gen
- ❌ **Type annotations often needed** - Scala can't always infer types, need `(tp: TestPeers)`
- ❌ **Ambiguous given instances** - Need explicit type parameters like `ReaderT.pure[Gen, TestPeers, A](value)`

## Comparison Summary

| Aspect | Old Approach (Explicit Context) | New Approach (ReaderT) |
|--------|--------------------------------|------------------------|
| **Call site verbosity** | ✅ Concise - simple lambdas | ❌ Verbose - ReaderT wrappers |
| **Implementation boilerplate** | ❌ Manual context threading | ✅ Automatic via monad |
| **Composability** | ❌ Hard to compose | ✅ Natural composition |
| **Learning curve** | ✅ Simple - just parameters | ❌ Requires monad knowledge |
| **Type inference** | ✅ Generally works well | ❌ Often needs annotations |
