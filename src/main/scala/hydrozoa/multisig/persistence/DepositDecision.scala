package hydrozoa.multisig.persistence

import hydrozoa.multisig.ledger.block.BlockNumber
import io.circe.syntax.*
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, Json}

/** A registered deposit's block-completion decision, stored in [[Cf.DepositDecisionIndex]] keyed by
  * the deposit request's id. A deposit is decided at the block that either absorbs it into the
  * treasury or rejects it; the deciding block is a later block than the deposit's registration
  * block, so it is not reachable from [[Cf.RequestBlockIndex]] and needs its own reverse index.
  *
  * Absence of a row means the deposit is still **undecided** (pending) — carried to a future block.
  *
  * The block-only value is sufficient: the absorbing settlement is derived from the block's Major
  * partition, and the post-dated refund from the deposit's registration partition.
  */
sealed trait DepositDecision:
    /** The block at which the decision was made. */
    def block: BlockNumber

object DepositDecision:
    /** Absorbed into the treasury by `block`'s settlement. */
    final case class Absorbed(block: BlockNumber) extends DepositDecision

    /** Rejected at `block` (not absorbed); the depositor is repaid by the post-dated refund. */
    final case class Rejected(block: BlockNumber) extends DepositDecision

    private val absorbed = "Absorbed"
    private val rejected = "Rejected"

    given Codec[DepositDecision] = Codec.from(
      Decoder.instance { c =>
          for {
              kind <- c.downField("kind").as[String]
              block <- c.downField("block").as[BlockNumber]
              decision <- kind match
                  case `absorbed` => Right(Absorbed(block))
                  case `rejected` => Right(Rejected(block))
                  case other =>
                      Left(DecodingFailure(s"unknown DepositDecision kind: $other", c.history))
          } yield decision
      },
      Encoder.instance { decision =>
          val kind = decision match
              case _: Absorbed => absorbed
              case _: Rejected => rejected
          Json.obj("kind" -> kind.asJson, "block" -> decision.block.asJson)
      }
    )
