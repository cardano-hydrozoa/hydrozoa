package hydrozoa.rulebased.ledger.l1.script.plutus.lib

import scalus.Compile
import scalus.ledger.api.v1.Value.{-, zero}
import scalus.ledger.api.v3.{CurrencySymbol, TokenName, Value}
import scalus.prelude.List.Cons
import scalus.prelude.{List, fail, require, given}

@Compile
object ValueExtension:
    extension (self: Value)

        /** Check - contains any amount of tokens under currency symbol provided.
          * @param cs
          * @return
          */
        def containsCurrencySymbol(cs: CurrencySymbol): Boolean =
            // Split away ada which always comes first
            self.toSortedMap.toList match
                case List.Cons(_, tokens) => tokens.map(_._1).contains(cs)
                case List.Nil             => false

        /** Check - contains only specified amount of same tokens and no other tokens
          * @param cs
          * @param tn
          * @param amount
          * @return
          */
        def containsExactlyOneAsset(
            cs: CurrencySymbol,
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
        def onlyNonAdaAsset: (CurrencySymbol, TokenName, BigInt) =
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
