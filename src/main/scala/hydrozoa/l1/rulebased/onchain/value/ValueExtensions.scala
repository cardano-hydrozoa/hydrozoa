package hydrozoa.l1.rulebased.onchain.value

import scalus.Compile
import scalus.ledger.api.v1.Value.{-, zero}
import scalus.ledger.api.v3.{CurrencySymbol, TokenName, Value}
import scalus.prelude.List
import scalus.prelude.List.Cons

@Compile
object ValueExtensions:

    extension (self: Value)
        // Check - contains only specified amount of same tokens and no other tokens
        def containsExactlyOneAsset(
            cs: CurrencySymbol,
            tn: TokenName,
            amount: BigInt
        ): Boolean =
            self.toList match
                case List.Cons(_, tokens) =>
                    tokens match
                        case List.Cons((cs_, assets), tail) =>
                            if tail.isEmpty then
                                if cs_ == cs then
                                    assets.toList match
                                        case List.Cons((tn_, amount_), tail) =>
                                            tail.isEmpty && tn_ == tn && amount_ == amount
                                        case _ => false
                                else false
                            else false
                        case _ => false
                case _ => false

        def onlyNonAdaToken: (CurrencySymbol, TokenName) = ???

        // Negate value, useful for burning operations
        def unary_- : Value = Value.zero - self
