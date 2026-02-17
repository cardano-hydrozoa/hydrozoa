package hydrozoa.lib.cardano.scalus.cardano.onchain.plutus

import scalus.Compile
import scalus.cardano.onchain.plutus.v1.Value.{-, zero}
import scalus.cardano.onchain.plutus.v3.{PolicyId, TokenName, Value}
import scalus.cardano.onchain.plutus.prelude.List.Cons
import scalus.cardano.onchain.plutus.prelude.{List, fail, require}

@Compile
object ValueExtension:
    extension (self: Value)

        /** Check - contains any number of tokens under the currency symbol provided.
          * @param cs
          * @return
          */
        def containsCurrencySymbol(cs: PolicyId): Boolean =
            // Split away ada which always comes first
            self.toSortedMap.toList match
                case List.Cons(_, tokens) => tokens.map(_._1).contains(cs)
                case List.Nil             => false

        /** Check - contains only specified number of same tokens and no other tokens
          * @param cs
          * @param tn
          * @param amount
          * @return
          */
        def containsExactlyOneAsset(
            cs: PolicyId,
            tn: TokenName,
            amount: BigInt
        ): Boolean =
            // Split away ada which always comes first
            self.toSortedMap.toList match
                case List.Cons(_, tokens) =>
                    tokens match
                        case List.Cons(symbol, otherSymbols) =>
                            if otherSymbols.isEmpty && symbol._1 == cs then
                                symbol._2.toList match
                                    case List.Cons(name, otherNames) =>
                                        otherNames.isEmpty && name._1 == tn && name._2 == amount
                                    case _ => false
                            else false
                        case _ => false
                case List.Nil => false

        /** Returns the only non-ada asset, i.e. a unique token in the value or fails.
          * @return
          */
        def onlyNonAdaAsset: (PolicyId, TokenName, BigInt) =
            // Split away ada which always comes first
            self.toSortedMap.toList match
                case List.Cons(_, tokens) =>
                    tokens match
                        case List.Cons((cs, names), otherSymbols) =>
                            require(
                              otherSymbols.isEmpty,
                              "onlyNonAdaToken: found more than one currency symbol"
                            )
                            names.toList match
                                case List.Cons((tokenName, amount), otherNames) =>
                                    require(
                                      otherNames.isEmpty,
                                      "onlyNonAdaToken: found more than one token name"
                                    )
                                    (cs, tokenName, amount)
                                case List.Nil => fail("onlyNonAdaToken: malformed value")
                        case List.Nil => fail("onlyNonAdaToken: no non-ada assets in value")
                case List.Nil =>
                    fail("onlyNonAdaToken: no non-ada assets in value")

        // Negate value, useful for burning operations
        def unary_- : Value = Value.zero - self
