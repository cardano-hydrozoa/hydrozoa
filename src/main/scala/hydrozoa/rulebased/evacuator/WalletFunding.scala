package hydrozoa.rulebased.evacuator

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.ledger.CollateralUtxo
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput
import scalus.cardano.txbuilder.TransactionBuilderStep.{Send, Spend}
import scalus.cardano.txbuilder.{Change, PubKeyWitness, TransactionBuilder}

/** Checks the wallet can pay for a whole evacuation before any of it is submitted, and consolidates
  * it when it cannot.
  *
  * An evacuation chain is funded by **one utxo, not by a balance**. The runner picks a single
  * collateral utxo and threads it through every transaction: each `Evacuate` spends it, pays its
  * fee out of it, and returns the remainder at output 0 for the next one. So a chain of `n`
  * transactions needs `fee × n` sitting in that one utxo, and a wallet holding many times that
  * across hundreds of small utxos still cannot fund it.
  *
  * The failure that causes is expensive because it is not a failure at launch: transactions land,
  * the chain stops partway, and the treasury is left part-drained. Recovery is safe — the runner is
  * stateless and rebuilds from chain state — but the fees are spent, and in a contested evacuation
  * the pause hands the unclaimed treasury back to whoever else is watching.
  */
object WalletFunding {

    /** Ada the chain will draw from its collateral utxo over its whole length.
      *
      * Every transaction in the chain is near-identical — same script, same batch size — so one
      * transaction's actual fee predicts the rest closely: measured across a 134-transaction run,
      * the mean was 1.3626 ada against 1.3647 for the first. That is why the preflight builds a
      * real transaction rather than estimating from protocol parameters.
      */
    def required(feePerTx: Coin, txCount: Int): Coin = Coin(feePerTx.value * txCount)

    /** How many transactions the given collateral can actually fund. Reported on failure so the
      * operator learns the size of the shortfall, not merely that there is one.
      */
    def fundableTxs(collateral: Coin, feePerTx: Coin): Long =
        if feePerTx.value <= 0 then Long.MaxValue else collateral.value / feePerTx.value

    /** The utxos to consolidate, and what they hold.
      *
      * Ada-only utxos are taken first, so a wallet that has enough plain ada never has to move
      * tokens at all; token-bearing ones are drawn on only once the plain ones run out.
      */
    final case class Selection(utxos: List[Utxo], ada: Coin, tokens: MultiAsset) {
        def isEmpty: Boolean = utxos.isEmpty
        def hasTokens: Boolean = tokens.assets.nonEmpty
    }

    def select(walletUtxos: Utxos, target: Coin): Selection = {
        val ordered = walletUtxos.toList
            .map((i, o) => Utxo(i, o))
            .sortBy(u => (u.output.value.assets.assets.nonEmpty, -u.output.value.coin.value))
        val (chosen, ada) = ordered
            .foldLeft((List.empty[Utxo], 0L)) { case ((acc, sum), u) =>
                if sum >= target.value then (acc, sum)
                else (u :: acc, sum + u.output.value.coin.value)
            }
        val tokens = chosen
            .map(_.output.value.assets)
            .foldLeft(MultiAsset.empty)(_ + _)
        Selection(chosen.reverse, Coin(ada), tokens)
    }

    /** Build one transaction that gathers `selection` into a single ada-only utxo.
      *
      * ★ The ada output must be **ada-only**: `CollateralUtxo.parse` rejects anything carrying
      * tokens, so a consolidation that swept tokens into the same output would produce a utxo the
      * runner cannot use as collateral — the exact problem it was meant to solve. Any tokens the
      * chosen inputs happen to carry are therefore sent to a second output, with just enough ada to
      * satisfy the min-ada rule.
      *
      * The ada output is index 0 so the balancer's change lands there, which is also where the
      * runner will look for its collateral.
      */
    def consolidationTx(
        selection: Selection,
        walletAddress: ShelleyAddress,
        params: ProtocolParams
    )(using network: CardanoNetwork.Section): Either[Throwable, Transaction] = {

        val tokenOutput: Option[TransactionOutput] =
            Option.when(selection.hasTokens) {
                // min-ada depends on the serialized size, which depends on the coin field — so
                // measure against a plausible coin and then set the answer, rather than measuring
                // against zero and under-paying.
                val probe = TransactionOutput.Babbage(
                  address = walletAddress,
                  value = Value(Coin(2_000_000L), selection.tokens),
                  datumOption = None,
                  scriptRef = None
                )
                val minCoin =
                    MinCoinSizedTransactionOutput.ensureMinAda(Sized(probe), params)
                probe.copy(value = Value(minCoin, selection.tokens))
            }

        val steps =
            selection.utxos.map(u => Spend(u, PubKeyWitness)) ++
                List(
                  Send(
                    TransactionOutput.Babbage(
                      address = walletAddress,
                      value = Value(Coin(0L)),
                      datumOption = None,
                      scriptRef = None
                    )
                  )
                ) ++ tokenOutput.map(Send.apply).toList

        for {
            ctx <- TransactionBuilder
                .build(network.network, steps)
                .left
                .map(e => RuntimeException(s"consolidation build failed: $e"))
            balanced <- ctx
                .balanceContext(
                  diffHandler = Change.changeOutputDiffHandler(
                    _,
                    _,
                    protocolParams = params,
                    changeOutputIdx = 0
                  ),
                  protocolParams = params,
                  evaluator = PlutusScriptEvaluator(
                    network.cardanoInfo,
                    EvaluatorMode.EvaluateAndComputeCost
                  )
                )
                .left
                .map(e => RuntimeException(s"consolidation balancing failed: $e"))
        } yield balanced.transaction
    }

    /** The collateral utxo a consolidation produced: output 0, by construction ada-only. */
    def collateralOf(tx: Transaction): Either[Throwable, CollateralUtxo] =
        CollateralUtxo
            .parse(Utxo(TransactionInput(tx.id, 0), tx.body.value.outputs.head.value))
            .left
            .map(e => RuntimeException(s"consolidation output 0 is not usable as collateral: $e"))
}
