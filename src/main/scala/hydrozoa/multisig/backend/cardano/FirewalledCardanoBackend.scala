package hydrozoa.multisig.backend.cardano

import cats.effect.IO
import hydrozoa.lib.logging.ContraTracer
import hydrozoa.multisig.backend.cardano.CardanoBackend.Error
import hydrozoa.multisig.ledger.l1.tx.EnrichedTx
import scalus.cardano.address.ShelleyAddress
import scalus.cardano.ledger.{AssetName, PolicyId, ProtocolParams, TransactionHash, TransactionInput, Utxo, Utxos}
import scalus.uplc.builtin.Data

/** Wraps a [[CardanoBackend]] and drops outbound [[submitTx]] calls when `shouldDrop` says so.
  * Every other method delegates unchanged. Emits
  * [[FirewalledCardanoBackendEvent.DroppedOutboundTx]] on drop.
  *
  * The predicate receives the full [[EnrichedTx]] so callers can dispatch on tx family / payload
  * (e.g. "drop only `FallbackTx` at major version 2") without resorting to tx-id observation.
  */
final class FirewalledCardanoBackend(
    underlying: CardanoBackend[IO],
    shouldDrop: EnrichedTx[?] => IO[Boolean],
    firewallTracer: ContraTracer[IO, FirewalledCardanoBackendEvent],
) extends CardanoBackend[IO]:

    // Never called: every abstract method below is overridden; the parent's default paths (which
    // are the only consumers of `tracer`) are bypassed.
    override protected def tracer: ContraTracer[IO, CardanoBackendEvent] =
        ContraTracer.nullTracer

    override def resolve(input: TransactionInput): IO[Either[Error, Option[Utxo]]] =
        underlying.resolve(input)

    override def utxosAt(address: ShelleyAddress): IO[Either[Error, Utxos]] =
        underlying.utxosAt(address)

    override def utxosAt(
        address: ShelleyAddress,
        asset: (PolicyId, AssetName),
    ): IO[Either[Error, Utxos]] =
        underlying.utxosAt(address, asset)

    override def isTxKnown(txHash: TransactionHash): IO[Either[Error, Boolean]] =
        underlying.isTxKnown(txHash)

    override def lastContinuingTxs(
        asset: (PolicyId, AssetName),
        after: TransactionHash,
    ): IO[Either[Error, List[(TransactionHash, Data, Data)]]] =
        underlying.lastContinuingTxs(asset, after)

    override def submitTx(etx: EnrichedTx[?]): IO[Either[Error, Unit]] =
        shouldDrop(etx).flatMap {
            case true =>
                firewallTracer
                    .traceWith(FirewalledCardanoBackendEvent.DroppedOutboundTx(etx.tx.id))
                    .as(Right(()))
            case false =>
                underlying.submitTx(etx).flatTap { result =>
                    firewallTracer.traceWith(
                      FirewalledCardanoBackendEvent.SubmittedTx(etx.tx.id, result)
                    )
                }
        }

    override def fetchLatestParams: IO[Either[Error, ProtocolParams]] =
        underlying.fetchLatestParams
