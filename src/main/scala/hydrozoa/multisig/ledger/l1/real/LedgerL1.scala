package hydrozoa.multisig.ledger.l1.real

type Address = String
type TxCbor = String
type CurrencySymbol = String
type Pkh = String
type ScalusTx = Any

import LedgerL1.{State, Tx}
import cats.effect.{IO, Ref}

/** This is a stand-in for Scalus' CBOR tx parser */
def txCborToScalus(tx: TxCbor): ScalusTx = ???

final case class LedgerL1(headAddress: Address, headCs: CurrencySymbol)(
    private val state: Ref[IO, State]
) {

    /** A transaction belongs to the head if it matches on address and currency symbol */
    def txBelongsToHead(tx: Tx.Serialized.HasTxCbor): Boolean =
        tx.headAddress == headAddress && tx.headCs == headCs

    trait StsError

    /** Check all L1 ledger rules except for the existence of the tx's inputs */
    def txValidateSts(tx: Tx.HasTxDeserialized): Either[StsError, Unit] =
        ???

    /** If the transaction passes [[txValidateSts]], apply it to transition the L1 ledger state. */
    def applyTx(tx: Tx): IO[Either[StsError, Unit]] =
        ???
}

object LedgerL1 {
    def create(headAddress: Address, headCs: CurrencySymbol): IO[LedgerL1] =
        for {
            state <- Ref[IO].of(State())
        } yield LedgerL1(headAddress, headCs)(state)

    sealed trait Utxo

    final case class TreasuryUtxo() extends Utxo
    final case class DepositUtxo() extends Utxo
    final case class RolloutUtxo() extends Utxo

    final case class State(
        treasury: Option[TreasuryUtxo] = None,
        deposits: List[DepositUtxo] = List(),
        rollouts: List[RolloutUtxo] = List()
    )

    sealed trait Tx extends Tx.Serialized.HasTxCbor, Tx.HasTxDeserialized, Tx.Unparse

    object Tx {
        final case class Initialization(
            treasuryProduced: TreasuryUtxo,
            override val headAddress: Address,
            override val headCs: CurrencySymbol,
            override val txCbor: TxCbor,
            override val txDeserialized: ScalusTx
        ) extends Tx {
            override type Serialized = Serialized.Initialization
            override def unparse: Serialized.Initialization =
                Serialized.Initialization(
                  headAddress = headAddress,
                  headCs = headCs,
                  txCbor = txCbor
                )
        }

        final case class Deposit(
            depositProduced: DepositUtxo,
            override val headAddress: Address,
            override val headCs: CurrencySymbol,
            override val txCbor: TxCbor,
            override val txDeserialized: ScalusTx
        ) extends Tx {
            override type Serialized = Serialized.Deposit
            override def unparse: Serialized.Deposit =
                Serialized.Deposit(headAddress = headAddress, headCs = headCs, txCbor = txCbor)
        }

        final case class Refund(
            depositSpent: DepositUtxo,
            override val headAddress: Address,
            override val headCs: CurrencySymbol,
            override val txCbor: TxCbor,
            override val txDeserialized: ScalusTx
        ) extends Tx {
            override type Serialized = Serialized.Refund
            override def unparse: Serialized.Refund =
                Serialized.Refund(headAddress = headAddress, headCs = headCs, txCbor = txCbor)
        }

        final case class Settlement(
            treasurySpent: TreasuryUtxo,
            treasuryProduced: TreasuryUtxo,
            depositsSpent: List[DepositUtxo],
            rolloutProduced: Option[Rollout],
            override val headAddress: Address,
            override val headCs: CurrencySymbol,
            override val txCbor: TxCbor,
            override val txDeserialized: ScalusTx
        ) extends Tx {
            override type Serialized = Serialized.Settlement
            override def unparse: Serialized.Settlement =
                Serialized.Settlement(headAddress = headAddress, headCs = headCs, txCbor = txCbor)
        }

        final case class Rollout(
            rolloutSpent: RolloutUtxo,
            rolloutProduced: Option[RolloutUtxo],
            override val headAddress: Address,
            override val headCs: CurrencySymbol,
            override val txCbor: TxCbor,
            override val txDeserialized: ScalusTx
        ) extends Tx {
            override type Serialized = Serialized.Rollout
            override def unparse: Serialized.Rollout =
                Serialized.Rollout(headAddress = headAddress, headCs = headCs, txCbor = txCbor)
        }

        final case class Fallback(
            treasurySpent: TreasuryUtxo,
            override val headAddress: Address,
            override val headCs: CurrencySymbol,
            override val txCbor: TxCbor,
            override val txDeserialized: ScalusTx
        ) extends Tx {
            override type Serialized = Serialized.Fallback
            override def unparse: Serialized.Fallback =
                Serialized.Fallback(headAddress = headAddress, headCs = headCs, txCbor = txCbor)
        }

        final case class Finalization(
            treasurySpent: TreasuryUtxo,
            override val headAddress: Address,
            override val headCs: CurrencySymbol,
            override val txCbor: TxCbor,
            override val txDeserialized: ScalusTx
        ) extends Tx {
            override type Serialized = Serialized.Finalization
            override def unparse: Serialized.Finalization =
                Serialized.Finalization(headAddress = headAddress, headCs = headCs, txCbor = txCbor)
        }

        sealed trait HasTxDeserialized {
            val txDeserialized: ScalusTx
        }

        sealed trait Unparse {
            type Serialized
            def unparse: Serialized
        }

        sealed trait Serialized extends Serialized.Parse, Serialized.HasTxCbor

        object Serialized {
            sealed trait Parse {
                type Parsed
                type ParseError
                def parse: Either[ParseError, Parsed]
            }

            sealed trait HasTxCbor {
                val headAddress: Address
                val headCs: CurrencySymbol
                val txCbor: TxCbor
            }

            sealed case class Initialization(
                override val headAddress: Address,
                override val headCs: CurrencySymbol,
                override val txCbor: TxCbor
            ) extends Serialized {
                import hydrozoa.multisig.ledger.l1.real.tx.InitializationTx
                override type Parsed = Tx.Initialization
                override type ParseError = InitializationTx.ParseError
                override def parse: Either[ParseError, Parsed] =
                    InitializationTx.parse(this)
            }

            sealed case class Deposit(
                override val headAddress: Address,
                override val headCs: CurrencySymbol,
                override val txCbor: TxCbor
            ) extends Serialized {
                import hydrozoa.multisig.ledger.l1.real.tx.DepositTx
                override type Parsed = Tx.Deposit
                override type ParseError = DepositTx.ParseError
                override def parse: Either[ParseError, Parsed] =
                    DepositTx.parse(this)
            }

            sealed case class Refund(
                override val headAddress: Address,
                override val headCs: CurrencySymbol,
                override val txCbor: TxCbor
            ) extends Serialized {
                import hydrozoa.multisig.ledger.l1.real.tx.RefundTx
                override type Parsed = Tx.Refund
                override type ParseError = RefundTx.ParseError
                override def parse: Either[ParseError, Parsed] =
                    RefundTx.parse(this)
            }

            sealed case class Settlement(
                override val headAddress: Address,
                override val headCs: CurrencySymbol,
                override val txCbor: TxCbor
            ) extends Serialized {
                import hydrozoa.multisig.ledger.l1.real.tx.SettlementTx
                override type Parsed = Tx.Settlement
                override type ParseError = SettlementTx.ParseError
                override def parse: Either[ParseError, Parsed] =
                    SettlementTx.parse(this)
            }

            sealed case class Rollout(
                override val headAddress: Address,
                override val headCs: CurrencySymbol,
                override val txCbor: TxCbor
            ) extends Serialized {
                import hydrozoa.multisig.ledger.l1.real.tx.RolloutTx
                override type Parsed = Tx.Rollout
                override type ParseError = RolloutTx.ParseError
                override def parse: Either[ParseError, Parsed] =
                    RolloutTx.parse(this)
            }

            sealed case class Fallback(
                override val headAddress: Address,
                override val headCs: CurrencySymbol,
                override val txCbor: TxCbor
            ) extends Serialized {
                import hydrozoa.multisig.ledger.l1.real.tx.FallbackTx
                override type Parsed = Tx.Fallback
                override type ParseError = FallbackTx.ParseError
                override def parse: Either[ParseError, Parsed] =
                    FallbackTx.parse(this)
            }

            sealed case class Finalization(
                override val headAddress: Address,
                override val headCs: CurrencySymbol,
                override val txCbor: TxCbor
            ) extends Serialized {
                import hydrozoa.multisig.ledger.l1.real.tx.FinalizationTx
                override type Parsed = Tx.Finalization
                override type ParseError = FinalizationTx.ParseError
                override def parse: Either[ParseError, Parsed] =
                    FinalizationTx.parse(this)
            }
        }
    }
}
