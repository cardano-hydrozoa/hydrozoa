package hydrozoa.integration.harness

import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.cardano.scalus.ledger.withZeroFees
import hydrozoa.lib.cardano.scalus.txbuilder.DiffHandler.prebalancedLovelaceDiffHandler
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.consensus.{UserRequest, UserRequestBody}
import hydrozoa.multisig.ledger.l1.token.CIP67
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.{AuxiliaryData, Coin, Metadatum, TransactionHash, TransactionInput, Utxo, Value, Word64}
import scalus.cardano.txbuilder.TransactionBuilderStep.{Fee, ModifyAuxiliaryData, Send, Spend}
import scalus.cardano.txbuilder.{PubKeyWitness, TransactionBuilder}
import scalus.uplc.builtin.Builtins.blake2b_256
import scalus.uplc.builtin.ByteString

/** The "kick" user request scenario tests submit to push `BlockWeaver` past a block boundary: a
  * well-formed, signed L2 tx spending a nonexistent input. It passes the ledger's stateless
  * screening — an unparseable payload would be rejected before ever reaching consensus — and
  * `JointLedger` marks it `Invalid` at apply (missing inputs), but the block still completes, which
  * is all the scenarios need.
  */
object KickRequest {

    /** Build the kick request, signed with `peerNum`'s wallet. */
    def mkKickTransactionRequest(
        multiNodeConfig: MultiNodeConfig,
        peerNum: HeadPeerNumber
    ): UserRequest.TransactionRequest = {
        val config = multiNodeConfig.headConfig
        val wallet = multiNodeConfig.nodeConfigs(peerNum).ownWallet
        val peerAddress = multiNodeConfig.addressOf(peerNum)
        val fabricatedInput = TransactionInput(
          TransactionHash.fromByteString(
            blake2b_256(ByteString.fromString("integration-harness block kick"))
          ),
          0
        )
        val output = Babbage(peerAddress, Value.ada(5), None, None)
        // The mandatory CIP-67 output-designation list: the single output stays on L2.
        val metadata = AuxiliaryData.Metadata(
          Map(Word64(CIP67.Tags.head) -> Metadatum.List(IndexedSeq(Metadatum.Int(2))))
        )
        val txSigned = TransactionBuilder
            .build(
              config.network,
              List(
                Spend(Utxo(fabricatedInput, output), PubKeyWitness),
                Send(output),
                Fee(Coin.zero),
                ModifyAuxiliaryData(_ => Some(metadata))
              )
            )
            .flatMap(
              _.finalizeContext(
                protocolParams = config.cardanoProtocolParams.withZeroFees,
                diffHandler = prebalancedLovelaceDiffHandler,
                evaluator = config.plutusScriptEvaluatorForTxBuild,
                validators = Seq.empty
              )
            )
            .fold(
              e => throw RuntimeException(s"could not build the kick tx: $e"),
              context => wallet.signTx(context.transaction)
            )
        UserRequest.TransactionRequest(
          UserRequestBody.TransactionRequestBody(
            l2Payload = ByteString.fromArray(txSigned.toCbor)
          )
        )
    }
}
