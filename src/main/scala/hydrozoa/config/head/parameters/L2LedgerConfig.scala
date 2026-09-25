package hydrozoa.config.head.parameters

import hydrozoa.lib.cardano.scalus.codecs.json.Codecs.{protocolParamsDecoder, protocolParamsEncoder}
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import scalus.cardano.ledger.ProtocolParams

/** The L2 ledger this head runs, together with whatever that ledger's peers agreed about it.
  *
  * The kind and its parameters are one value rather than two fields, so they cannot disagree: there
  * is no way to configure a `cardano-eutxo` head without the parameters its ledger validates
  * against, and no way to hand a remote head parameters that mean nothing to it.
  *
  * **[[HeadParameters]] stays ledger-agnostic by holding this and not looking inside.** There is no
  * bound on how many remote L2 ledgers exist, so the shared head parameters cannot grow a field per
  * backend — `ProtocolParams` is a Cardano type, and a head driving an order book has no use for
  * one. Each kind carries its own agreed parameters here, on its own branch.
  */
enum L2LedgerConfig:
    /** The built-in EUTXO reference ledger, run in-process by every node.
      *
      * @param protocolParams
      *   the **L2** protocol parameters, snapshotted from the network at `build-head-config` and
      *   then fixed for the head's life. Not the L1 set: that one tracks the chain and moves with
      *   every hard fork, while this must never move, because `l2ParamsHash` commits to it in the
      *   multisig regime datum. See `docs/spec/head-params-hash.md`.
      */
    case CardanoEutxo(protocolParams: ProtocolParams)

    /** A remote L2 ledger (e.g. SugarRush) each node reaches over its own `remoteLedgerUri`.
      *
      * Carries no parameters: a remote ledger's are its own and never cross into the head config.
      * What the head holds instead is `l2ParamsHash`, the digest the operator copies across —
      * GUM-342.
      */
    case AnyRemote

object L2LedgerConfig {

    extension (self: L2LedgerConfig)
        /** Which ledger this is, without its parameters — the discriminator the CLI's `--l2-ledger`
          * flag selects and every config file names.
          */
        def kind: L2LedgerKind = self match {
            case _: L2LedgerConfig.CardanoEutxo => L2LedgerKind.CardanoEutxo
            case L2LedgerConfig.AnyRemote       => L2LedgerKind.AnyRemote
        }

        /** The name this ledger carries in every config file and on the wire. */
        def configString: String = self.kind.configString

    private val kindField = "kind"
    private val protocolParamsField = "protocolParams"

    given Encoder[L2LedgerConfig] = Encoder.instance {
        case L2LedgerConfig.CardanoEutxo(protocolParams) =>
            Json.obj(
              kindField -> Json.fromString(L2LedgerKind.CardanoEutxo.configString),
              protocolParamsField -> protocolParams.asJson
            )
        case L2LedgerConfig.AnyRemote =>
            Json.obj(kindField -> Json.fromString(L2LedgerKind.AnyRemote.configString))
    }

    given Decoder[L2LedgerConfig] = Decoder.instance { cursor =>
        cursor.downField(kindField).as[L2LedgerKind].flatMap {
            case L2LedgerKind.CardanoEutxo =>
                cursor
                    .downField(protocolParamsField)
                    .as[ProtocolParams]
                    .map(L2LedgerConfig.CardanoEutxo.apply)
            case L2LedgerKind.AnyRemote => Right(L2LedgerConfig.AnyRemote)
        }
    }
}
