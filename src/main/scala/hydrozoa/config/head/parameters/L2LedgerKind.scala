package hydrozoa.config.head.parameters

import io.circe.{Decoder, Encoder}

/** Which L2 ledger a head runs. This is agreed by every peer (it lives in [[HeadParameters]]): an
  * in-process EUTXO ledger and a remote black box have very different trust models, so peers must
  * not disagree on it. Each node then instantiates the matching implementation — see `Main` — and a
  * remote ledger additionally needs its own (per-node) `remoteLedgerUri`.
  */
enum L2LedgerKind:
    /** The built-in EUTXO reference ledger, run in-process by every node. */
    case CardanoEutxo

    /** A remote L2 ledger (e.g. SugarRush) each node reaches over its own `remoteLedgerUri`. */
    case AnyRemote

object L2LedgerKind {
    private val cardanoEutxo = "cardano-eutxo"
    private val anyRemote = "any-remote"

    extension (self: L2LedgerKind)
        /** The name this kind carries in every config file and on the wire. Single-sourced here so
          * the JSON codec and [[hydrozoa.config.head.HeadParamsHash]] agree by construction.
          */
        def configString: String = self match {
            case L2LedgerKind.CardanoEutxo => cardanoEutxo
            case L2LedgerKind.AnyRemote    => anyRemote
        }

    given Encoder[L2LedgerKind] = Encoder.encodeString.contramap(_.configString)

    given Decoder[L2LedgerKind] = Decoder.decodeString.emap {
        case `cardanoEutxo` => Right(L2LedgerKind.CardanoEutxo)
        case `anyRemote`    => Right(L2LedgerKind.AnyRemote)
        case other =>
            Left(s"unknown l2Ledger '$other' (expected '$cardanoEutxo' or '$anyRemote')")
    }
}
