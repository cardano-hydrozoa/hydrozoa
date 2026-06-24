package hydrozoa.multisig.ledger.eutxol2.store

import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.{utxoDecoder, utxoEncoder}
import hydrozoa.multisig.ledger.eutxol2.tx.L2Genesis
import hydrozoa.multisig.ledger.event.RequestId
import hydrozoa.multisig.ledger.l2.{Destination, L2CommandNumber, L2LedgerCommand}
import io.bullet.borer.Cbor
import io.circe.generic.semiauto.deriveCodec
import io.circe.syntax.*
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, Json}
import scala.util.Try
import scalus.cardano.ledger.{Utxo, Utxos}
import scalus.uplc.builtin.ByteString

/** JSON codecs for the values the RocksDB [[RocksDbL2Store]] persists: the logged
  * [[L2LedgerCommand.Real]] (the log) and the [[L2Snapshot]] (the restore accelerator). The
  * in-memory store needs none of this; these exist only for the on-disk byte form (§R2b).
  *
  * These are deliberately *store-local* and not the codecs `L2LedgerCommand` exports for the
  * SugarRush wire: those rename fields (`userVKey` → `userVk`) and encode `Destination` as a lossy
  * object, so they do not round-trip (see the FIXMEs in `L2LedgerCommand`). Persistence needs exact
  * round-trip, so the command subtypes are re-derived symmetrically here, and the Borer-only leaves
  * (`Destination`, `L2Genesis`) go through a CBOR-hex string — the same shape the Scalus ledger
  * leaves already use.
  */
object L2StoreCodecs:

    import hydrozoa.lib.cardano.cip116.JsonCodecs.CIP0116.Conway.given
    // The L2 ledger uses the i64 wire form of RequestId (SugarRush u64), not the default object.
    import hydrozoa.multisig.ledger.event.RequestId.i64.given
    import hydrozoa.multisig.ledger.block.BlockNumber.given

    /** A round-tripping Circe codec for a type that has Borer codecs, via a CBOR-hex string. */
    private def cborHexCodec[A](using
        io.bullet.borer.Encoder[A],
        io.bullet.borer.Decoder[A]
    ): Codec[A] =
        Codec.from(
          Decoder.decodeString.emap(hex =>
              Try(Cbor.decode(ByteString.fromHex(hex).bytes).to[A].value).toEither.left
                  .map(e => s"invalid CBOR-hex: ${e.getMessage}")
          ),
          Encoder.encodeString.contramap(a =>
              ByteString.fromArray(Cbor.encode(a).toByteArray).toHex
          )
        )

    private given Codec[Destination] = { import Destination.given; cborHexCodec[Destination] }
    private given Codec[L2Genesis] = {
        import hydrozoa.multisig.ledger.eutxol2.tx.given
        cborHexCodec[L2Genesis]
    }

    // -- L2LedgerCommand.Real (the log value) --------------------------------

    private given Codec[L2LedgerCommand.RegisterDeposit] = deriveCodec
    private given Codec[L2LedgerCommand.ApplyDepositDecisions] = deriveCodec
    private given Codec[L2LedgerCommand.ApplyTransaction] = deriveCodec

    private val RegisterDepositTag = "RegisterDeposit"
    private val ApplyDepositDecisionsTag = "ApplyDepositDecisions"
    private val ApplyTransactionTag = "ApplyTransaction"

    given realCommandCodec: Codec[L2LedgerCommand.Real] = Codec.from(
      Decoder.instance(c =>
          c.keys
              .flatMap(_.headOption)
              .toRight(DecodingFailure("L2 command must have exactly one tag field", c.history))
              .flatMap {
                  case RegisterDepositTag =>
                      c.get[L2LedgerCommand.RegisterDeposit](RegisterDepositTag)
                  case ApplyDepositDecisionsTag =>
                      c.get[L2LedgerCommand.ApplyDepositDecisions](ApplyDepositDecisionsTag)
                  case ApplyTransactionTag =>
                      c.get[L2LedgerCommand.ApplyTransaction](ApplyTransactionTag)
                  case other =>
                      Left(DecodingFailure(s"Unknown L2 command tag: $other", c.history))
              }
      ),
      Encoder.instance {
          case c: L2LedgerCommand.RegisterDeposit => Json.obj(RegisterDepositTag -> c.asJson)
          case c: L2LedgerCommand.ApplyDepositDecisions =>
              Json.obj(ApplyDepositDecisionsTag -> c.asJson)
          case c: L2LedgerCommand.ApplyTransaction => Json.obj(ApplyTransactionTag -> c.asJson)
      }
    )

    // -- L2Snapshot (the snapshot value) -------------------------------------

    /** `Utxos` (`Map[TransactionInput, TransactionOutput]`) as a `List[Utxo]` — same value-side
      * pattern as `FoundationCodecs.resolvedUtxos` (avoids a `KeyEncoder[TransactionInput]`).
      */
    private given Codec[Utxos] = Codec.from(
      Decoder.decodeList[Utxo].map(_.iterator.map(u => u.input -> u.output).toMap),
      Encoder.encodeList[Utxo].contramap(_.iterator.map((i, o) => Utxo(i, o)).toList)
    )

    given snapshotCodec: Codec[L2Snapshot] = Codec.from(
      Decoder.instance(c =>
          for
              commandNumber <- c.downField("commandNumber").as[Long]
              activeUtxos <- c.downField("activeUtxos").as[Utxos]
              // pendingDeposits: list of (RequestId, L2Genesis) — no KeyEncoder[RequestId] needed.
              pendingDeposits <- c.downField("pendingDeposits").as[List[(RequestId, L2Genesis)]]
          yield L2Snapshot(L2CommandNumber(commandNumber), activeUtxos, pendingDeposits.toMap)
      ),
      Encoder.instance(s =>
          Json.obj(
            "commandNumber" -> (s.commandNumber: Long).asJson,
            "activeUtxos" -> s.activeUtxos.asJson,
            "pendingDeposits" -> s.pendingDeposits.toList.asJson
          )
      )
    )
