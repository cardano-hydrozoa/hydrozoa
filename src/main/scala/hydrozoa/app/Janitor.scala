package hydrozoa.app

import cats.effect.IO
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.config.head.HeadConfig
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.config.head.network.CardanoNetwork.ensureMinAda
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.peer.HeadPeerWallet
import scala.collection.immutable.SortedMap
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{AssetName, Coin, EvaluatorMode, MultiAsset, PlutusScriptEvaluator, TransactionOutput, Utxo, Utxos, Value}
import scalus.cardano.txbuilder.TransactionBuilderStep.{Mint, Send, Spend}
import scalus.cardano.txbuilder.{Change, TransactionBuilder}

/** This is only useful until we teach the runnable to handle the finalization correctly.
  */
object Janitor:

    val logger = Logging.loggerIO("hydrozoa.app.Janitor")

    /** Upon the process finalization tries to build and submit a tx that:
      *   - grabs all utxos at [[headAddress]]
      *   - burns all token under [[headPolicy]]
      *   - sends all ADA to [[faucetAddress]]
      *   - sends all non-head tokens to [[tokenRecoveryAddress]] (if defined)
      *   - is signed by [[headPeerWallet]]
      *
      * @param backend
      * @param headPeerWallet
      * @param config
      * @param faucetAddress
      * @param tokenRecoveryAddress
      */
    def cleanUp(
        backend: CardanoBackend[IO],
        config: HeadConfig,
        headPeerWallet: HeadPeerWallet,
        faucetAddress: ShelleyAddress,
        tokenRecoveryAddress: Option[ShelleyAddress]
    ): IO[Unit] = for {

        headUtxos: Utxos <- backend
            .utxosAt(config.headMultisigAddress)
            .flatMap(_.fold(IO.raiseError, IO.pure))

        _ <- IO.whenA(headUtxos.nonEmpty) {
            for {

                _ <- logger.info(
                  s"Found ${headUtxos.size} utxo(s) at the head multisig address, cleaning up..."
                )

                totalValue = Value.combine(headUtxos.map((_, o) => o.value))

                headTokens = totalValue.assets.assets.getOrElse[SortedMap[AssetName, Long]](
                  key = config.headMultisigScript.policyId,
                  default = SortedMap.empty
                )

                headTokensValue =
                    if headTokens.nonEmpty
                    then
                        Value(
                          coin = Coin.zero,
                          assets = MultiAsset(
                            SortedMap(config.headMultisigScript.policyId -> headTokens)
                          )
                        )
                    else Value.zero

                // Calculate non-head tokens (all tokens except head tokens)
                nonHeadTokensAssets = totalValue.assets.assets - config.headMultisigScript.policyId
                nonHeadTokensValue = Value(Coin.zero, MultiAsset(nonHeadTokensAssets))

                // NB: for a multisig head it's always true
                hasMultisigRefScript = true
                // hasMultisigRefScript = headUtxos.exists((_, o) =>
                //    o.scriptRef.contains(ScriptRef(config.headMultisigScript._1))
                // )

                (withRefScript, withoutRefScript) = headUtxos.partition((_, o) =>
                    o.scriptRef.isDefined
                )

                // Build outputs based on whether we need to send tokens separately
                outputs = (tokenRecoveryAddress, nonHeadTokensValue.assets.nonEmpty) match {
                    case (Some(recoveryAddr), true) =>
                        // Create token output with min ADA
                        val tokenOutput = TransactionOutput
                            .Babbage(
                              address = recoveryAddr,
                              value = nonHeadTokensValue
                            )
                            .ensureMinAda(config)

                        val minAdaForTokens = tokenOutput.value.coin

                        // Send remaining ADA to faucet
                        val adaOutput = TransactionOutput.Babbage(
                          address = faucetAddress,
                          value = Value(totalValue.coin - minAdaForTokens, MultiAsset.empty)
                        )

                        List(Send(adaOutput), Send(tokenOutput))

                    case _ =>
                        // Send all (ADA + any tokens) to faucet, minus burned head tokens
                        List(
                          Send(
                            TransactionOutput.Babbage(
                              address = faucetAddress,
                              value = totalValue - headTokensValue
                            )
                          )
                        )
                }

                unbalanced = TransactionBuilder
                    .build(
                      config.cardanoNetwork.cardanoInfo.network,
                      // Utxos with ref should come first
                      withRefScript.map { case (utxoId, output) =>
                          Spend(
                            utxo = Utxo(utxoId, output),
                            witness = config.headMultisigScript.witnessAttached
                          )
                      }.toList ++
                          withoutRefScript.map { case (utxoId, output) =>
                              Spend(
                                utxo = Utxo(utxoId, output),
                                witness = config.headMultisigScript.witnessAttached
                              )
                          }.toList ++
                          headTokens
                              .map((assetName, amount) =>
                                  Mint(
                                    scriptHash = config.headMultisigScript.policyId,
                                    assetName = assetName,
                                    amount = -amount,
                                    witness = config.headMultisigScript.witnessAttached
                                  )
                              )
                              .toList ++
                          outputs
                    )
                    .fold(err => throw RuntimeException(err.toString), identity)

                balanced = unbalanced
                    .balanceContext(
                      diffHandler = Change.changeOutputDiffHandler(
                        _,
                        _,
                        protocolParams = config.cardanoNetwork.cardanoProtocolParams,
                        changeOutputIdx = 0
                      ),
                      protocolParams = config.cardanoNetwork.cardanoProtocolParams,
                      evaluator = PlutusScriptEvaluator(
                        config.cardanoNetwork.cardanoInfo,
                        EvaluatorMode.EvaluateAndComputeCost
                      )
                    )
                    .fold(err => throw RuntimeException(err.toString), _.transaction)

                signed = headPeerWallet.signTx(balanced)

                _ <- logger.info(s"clean-up tx: ${HexUtil.encodeHexString(signed.toCbor)}")

                ret <- backend.submitTx(signed)

                _ <- logger.info(s"submission result: $ret")
            } yield ()
        }
    } yield ()
