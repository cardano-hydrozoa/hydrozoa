package hydrozoa.rulebased

import cats.*
import cats.effect.*
import cats.syntax.all.*
import com.suprnation.actor.Actor.*
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.backend.cardano.CardanoBackend
import hydrozoa.multisig.consensus.peer.PeerWallet
import hydrozoa.multisig.ledger.block.BlockHeader
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67.TokenNames
import hydrozoa.{L1, UtxoSetL2, VerificationKeyBytes, rulebased}
import scala.concurrent.duration.FiniteDuration
import scalus.cardano.ledger.{CardanoInfo, TransactionHash}

/** This doesn't actually do much of anything right now. It just starts the dispute and liquidation
  * actors, and those proceed autonomously. I don't think we need actors for these.
  */
case class RuleBasedRegimeManager(
    config: RuleBasedRegimeManager.Config,
    collateralUtxo: hydrozoa.Utxo[L1],
    blockHeader: BlockHeader.Minor.Onchain,
    signatures: List[BlockHeader.Minor.HeaderSignature],
    cardanoBackend: CardanoBackend[IO],
    votingDeadline: QuantizedInstant,
    utxosToWithdrawL2: UtxoSetL2,
    l2SetAtFallback: UtxoSetL2,
    fallbackTxHash: TransactionHash
) extends Actor[IO, Unit] {

    // Start the dispute and liquidation actors
    override def preStart: IO[Unit] = {
        import config.*
        val disputeActorConfig: DisputeActor.Config = DisputeActor.Config(
          ownPeerPkh = ownPeerPkh,
          tokenNames = tokenNames,
          headMultisigScript = headMultisigScript,
          receiveTimeout = receiveTimeout,
          cardanoInfo = cardanoInfo
        )
        val liquidationActorConfig: LiquidationActor.Config = LiquidationActor.Config(
          withdrawalFeeWallet = withdrawalFeeWallet,
          receiveTimeout = receiveTimeout,
          headMultisigScript = headMultisigScript,
          tokenNames = tokenNames,
          cardanoInfo = cardanoInfo
        )

        for {
            _ <- context.actorOf(
              DisputeActor(
                collateralUtxo = collateralUtxo,
                blockHeader = blockHeader,
                signatures = signatures,
                config = disputeActorConfig,
                cardanoBackend = cardanoBackend,
              )
            )
            _ <- context.actorOf(
              LiquidationActor(
                allUtxosToWithdraw = utxosToWithdrawL2,
                cardanoBackend = cardanoBackend,
                config = liquidationActorConfig,
                l2SetAtFallback = l2SetAtFallback,
                fallbackTxHash = fallbackTxHash
              )
            )
        } yield ()
    }
}

object RuleBasedRegimeManager {
    case class Config(
        ownPeerPkh: VerificationKeyBytes,
        tokenNames: TokenNames,
        cardanoInfo: CardanoInfo,
        withdrawalFeeWallet: PeerWallet,
        receiveTimeout: FiniteDuration,
        headMultisigScript: HeadMultisigScript,
    )
}
