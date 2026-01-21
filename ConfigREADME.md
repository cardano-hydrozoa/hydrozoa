- HeadConfig is a huge top level config that we pass around.
- Why?
  - Because we couldn't find a clean solution to structural typing/row polymorphism in scala that:
    - allowed us to avoid making intermediate case classes and projection functions for every component that should receive a subset of the head config
    - gave us good compilation errors -- we want the compiler to tell us WHICH fields are missing, not just give us a long list and tell us there is a type mismatch
    - is "composable", in the sense that if component `A` needs to contain the union of the configuration of depedent components `B` and `C`, as well as some of its own configuration, we don't get into weird path-dependency issues where scala can't determine where and how the configuration overlap and needs to be told explicitly
    - allows us to use type-safe anonymous structures (i.e., anonymous classes) that can be combined with a basic algebra
    - allows us to use types that can be combined with a basic algebra (i.e., row types)
    - doesn't require a dependency like `shapeless` that would then become a pre-requisite for on-boarding new developers
    - doesn't require custom macros
    - enforces naming consistency such that we don't end up with things like `tallyFeeAllowance` and `allowanceForTallyFee`

- How we use it:
  - The HeadConfig is built as a result of initializing a head. It requires parsing information in the configuration bundle to be 
    valid.
  - We then pass this around to different components, such as tx builders and actors. The companion objects have `def apply(config : HeadConfig, (...))` functions defined that serve to separate out static configuration from dynamic arguments.
  - Within components that need access to the configuration, we write a top-level `import config.{a, b, c}` to indicate which parts of the configuration are _actually_ necessary for the component. Inline field extraction such as `config.a` should be avoided by default, as should wildcard imports such as `import config.*`
    

TxBuilders:

- Each TxBuilder should be parameterized on a `config : HeadConfig` and `validators : Seq[Validator]`
  - These SHOULD be passed ONLY ONCE in each file.
    - If you're finding yourself passing a value of type `config : HeadConfig` or some subset of the fields of `HeadConfig` in multiple 
      functions, it is probably wrong.

Actors:

- Each actor should be paramterized on a `config : HeadConfig`
  - These SSHOULD be passed only once in each file
    - If you're finding yourself passing a value of type `config : HeadConfig` or some subset of fields of the `HeadConfig` in multiple 
      functions, it is probably wrong.


-----


TxBuilder Architectures:


# DeinitTx
Current:
- case class DeInitTx extends Tx[DeinitTx]
- Object DeInitTx
  - case class Builder(config, (...))
  - case class Steps(config , (...))

# DepositTx
- case class DepositTx extends Tx[DepositTx]
- Object Deposit
  - case class Builder(config) extends Tx.Builder
  - sealed trait ParseError extends Throwable
  - def parse
 
# Fallbaack
- case class FallbackTx extends Tx
- object FallbackTx
  - def build(recipe : Recipe) 
  - case class Recipe(config (...))

# Finalization
- sealed trait Finalization extends Tx[FinalizationTx, (...field accessors...)
- object FinalizationTx
  - sealed trait $TRANSACTION_VARIANT extends FinalizationTx
  - case class $OTHER_TRANSACTION_VARIANT extends $TRANSACTION_VARIANT
  - object Builder
    - def upgrade(config, ...)
    - object Args
      - sealed trait Some
      - case class $ARGS_VARIANT
    - sealed trait PartialResult extends (... field accessors ...)
      - def foo(config, ...)
      - def bar(config, ...)
    - object Partial Result
      - case class $PARTIAL_RESULT_VARIANT(...)
        - def foo (config, ...)
        - def bar (config, ...)
      
# Initialization
- case class InitializationTx
- object InitializationTx
  - def build (recipe : Recipe)
  - def parse (config, (...))
  - sealed trait ParseError extends Throwble
  - case class Recipe(config, (...))  

# Refund
- trait RefundTx
- object RefundTx
  - case class $TRANSACTION_VARIANT extends RefundTx, Tx[$VARIANT]
  - object Builder
    - case class $VARIANT(config) extends Builder[Variant]
  - sealed trait PartialResult
    - def foo(config, (...))
  - object PartialResult
    - case class $RESULT_VARIANT extends PartialResult[$RESULT_VARIANT]
  - trait Builder [$VARIANT] extends Tx.Builder
  - private object BuilderOps
  - sealed trait ParseError
  - def parse(config, (...))

# RolloutTx 

- sealed trait RolloutTx extends Tx[RolloutTx], (field accessors)
- object RolloutTx
  - case class $TX_VARIANT extends RolloutTx
  - object Builder
    - case class $BUILDER_VARIANT(config) extends Builder[$Variant]
      - override type ArgsType = Args.Variant
  - trait PartialResult[$Variant]
    - def builder
  - object PartialResult
    - case class $Variant(...) extends PartialResult
  - enum Args
  - case class State
  - object State
  - trait Builder[$VARIANT] extends Tx.Builder
    - type ArgsType <: Args
    - def foo(args : ArgsType)
  - private object BuilderOps   
    - def foo(config)
    - def bar(config)
  - object RolloutOutput
    - def foo(config)
    - def bar(config)
  - object SpendRollout
    - def foo(config)   
  - object Placeholder
    - def foo (builder)
    - def bar (builder)
    - def foo1 (config)
    - def foo2 (config)

# SettlementTx

- sealed trait SettlementTx extends Tx[SettlementTx], (field accessors)
- object SettlementTx
  - import Builder.*
  - import BuilderOps.*
  - sealed trait $VARIANT extends SettlementTx
  - case class $VARIANT_2 extends $VARIANT
  - object Builder
    - final case class Variant2(config) extends Builder[$Variant_2]
      - override type ArgsType
      - override type ResultType
      - def complete(args : ArgsType): BuildErrorOr[ResultType]
    - trait Result
    - object Result
      - sealed trait $RESULT_VARIANT
      - case class $RESULT_VARIANT extends $RESULT_VARINT
    - trait Args
    - object Args
      - case class Arg(config) extends Args
  - trait Builder[$VARIANT]
    - type ArgsType 
    - type ResultType
    - def foo(args, ...)
    - def bar(args, ...)
    - private object whatever
      - def foo(config, args)
      - def bar(config)
      - def baz(config, ...)
      - def qux(args)    


# Rollout Tx Example

Goals:
  - Pass config once at the top.
    - To accomplish this: Move everything that needs the configuration into the Builder trait 

- sealed trait RolloutTx extends Tx[RolloutTx], (field accessors)
- object RolloutTx
  - case class $TX_VARIANT extends RolloutTx
  - object Builder
    - case class $BUILDER_VARIANT(config) extends Builder[$Variant]
      - override type ArgsType = Args.Variant
  - trait PartialResult[$Variant]
    - def builder
  - object PartialResult
    - case class $Variant(...) extends PartialResult
  - enum Args
  - case class State
  - object State
  - trait Builder[$VARIANT] extends Tx.Builder
    - type ArgsType <: Args
    - def foo(args : ArgsType)
    - private object BuilderOps   
      - def foo
      - def bar
    - object RolloutOutput
      - def foo
      - def bar
    - object SpendRollout
      - def foo   
    - object Placeholder
      - def foo (builder)
      - def bar (builder)
      - def foo1
      - def foo2



