package hydrozoa.app

import cats.effect.IO
import com.bloxbean.cardano.client.util.HexUtil
import hydrozoa.config.head.HeadConfig
import hydrozoa.lib.logging.Logging
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.peer.HeadPeerWallet
import scala.collection.immutable.SortedMap
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{AssetName, Coin, EvaluatorMode, MultiAsset, PlutusScriptEvaluator, ScriptRef, TransactionOutput, Utxo, Utxos, Value}
import scalus.cardano.txbuilder.TransactionBuilderStep.{Mint, Send, Spend}
import scalus.cardano.txbuilder.{Change, TransactionBuilder}

/** This is only useful until we teach the runnable to handle the finalization correctly.
  */
object Janitor:

    val logger = Logging.loggerIO("hydrozoa.app.Janitor")

    /** Upon the process finalization tries to build and submit a tx that:
      *   - grabs all utxos at [[headAddress]]
      *   - burns all token under [[headPolicy]]
      *   - send all residual value to [[faucetAddress]]
      *   - is signed by [[headPeerWallet]]
      *
      * @param backend
      * @param headPeerWallet
      * @param headAddress
      * @param faucetAddress
      */
    def cleanUp(
        backend: CardanoBackend[IO],
        config: HeadConfig,
        headPeerWallet: HeadPeerWallet,
        faucetAddress: ShelleyAddress
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

                hasMultisigRefScript = headUtxos.exists((_, o) =>
                    o.scriptRef.contains(ScriptRef(config.headMultisigScript._1))
                )

                unbalanced = TransactionBuilder
                    .build(
                      config.cardanoNetwork.cardanoInfo.network,
                      headUtxos.map { case (utxoId, output) =>
                          Spend(
                            utxo = Utxo(utxoId, output),
                            witness =
                                if hasMultisigRefScript then
                                    config.headMultisigScript.witnessAttached
                                else config.headMultisigScript.witnessValue
                          )
                      }.toList ++
                          (headTokens
                              .map((assetName, amount) =>
                                  Mint(
                                    scriptHash = config.headMultisigScript.policyId,
                                    assetName = assetName,
                                    amount = -amount,
                                    witness = config.headMultisigScript.witnessAttached
                                  )
                              )
                              .toList :+
                              Send(
                                TransactionOutput.Babbage(
                                  address = faucetAddress,
                                  value = totalValue - headTokensValue
                                )
                              ))
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
