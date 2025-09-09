package hydrozoa.multisig.ledger.l1.real

type Address = String
type TxCbor = String
type CurrencySymbol = String
type Pkh = String

import LedgerL1.{State, Tx}
import cats.effect.{IO, Ref}

final case class LedgerL1(headAddress: Address, headCs: CurrencySymbol)(
    private val state: Ref[IO, State]
) {
    def txBelongsToHead(tx: Tx.Serialized.WithCbor): Boolean =
        tx.headAddress == headAddress && tx.headCs == headCs

    trait StsError

    def txValidateSts(tx: Tx.Serialized.WithCbor): Either[StsError, Unit] =
        ???

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

    sealed trait Tx extends Tx.Serialized.WithCbor, Tx.Unparse

    object Tx {
        final case class Initialization(
            treasuryProduced: TreasuryUtxo,
            override val headAddress: Address,
            override val headCs: CurrencySymbol,
            override val txCbor: TxCbor
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
            override val txCbor: TxCbor
        ) extends Tx {
            override type Serialized = Serialized.Deposit
            override def unparse: Serialized.Deposit =
                Serialized.Deposit(headAddress = headAddress, headCs = headCs, txCbor = txCbor)
        }

        final case class Refund(
            depositSpent: DepositUtxo,
            override val headAddress: Address,
            override val headCs: CurrencySymbol,
            override val txCbor: TxCbor
        ) extends Tx {
            override type Serialized = Serialized.Refund
            override def unparse: Serialized.Refund =
                Serialized.Refund(headAddress = headAddress, headCs = headCs, txCbor = txCbor)
        }

        final case class Settlement(
            depositsSpent: List[DepositUtxo],
            treasuryProduced: TreasuryUtxo,
            rolloutProduced: Option[Rollout],
            override val headAddress: Address,
            override val headCs: CurrencySymbol,
            override val txCbor: TxCbor
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
            override val txCbor: TxCbor
        ) extends Tx {
            override type Serialized = Serialized.Rollout
            override def unparse: Serialized.Rollout =
                Serialized.Rollout(headAddress = headAddress, headCs = headCs, txCbor = txCbor)
        }

        final case class Fallback(
            treasurySpent: TreasuryUtxo,
            override val headAddress: Address,
            override val headCs: CurrencySymbol,
            override val txCbor: TxCbor
        ) extends Tx {
            override type Serialized = Serialized.Fallback
            override def unparse: Serialized.Fallback =
                Serialized.Fallback(headAddress = headAddress, headCs = headCs, txCbor = txCbor)
        }

        final case class Finalization(
            treasurySpent: TreasuryUtxo,
            override val headAddress: Address,
            override val headCs: CurrencySymbol,
            override val txCbor: TxCbor
        ) extends Tx {
            override type Serialized = Serialized.Finalization
            override def unparse: Serialized.Finalization =
                Serialized.Finalization(headAddress = headAddress, headCs = headCs, txCbor = txCbor)
        }

        sealed trait Unparse {
            type Serialized
            def unparse: Serialized
        }

        sealed trait Serialized extends Serialized.Parse, Serialized.WithCbor

        object Serialized {
            sealed trait Parse {
                type Deserialized
                type ParseError
                def parse: Either[ParseError, Deserialized]
            }

            sealed trait WithCbor {
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
                override type Deserialized = Tx.Initialization
                override type ParseError = InitializationTx.ParseError
                override def parse: Either[ParseError, Deserialized] =
                    InitializationTx.parse(this)
            }

            sealed case class Deposit(
                override val headAddress: Address,
                override val headCs: CurrencySymbol,
                override val txCbor: TxCbor
            ) extends Serialized {
                import hydrozoa.multisig.ledger.l1.real.tx.DepositTx
                override type Deserialized = Tx.Deposit
                override type ParseError = DepositTx.ParseError
                override def parse: Either[ParseError, Deserialized] =
                    DepositTx.parse(this)
            }

            sealed case class Refund(
                override val headAddress: Address,
                override val headCs: CurrencySymbol,
                override val txCbor: TxCbor
            ) extends Serialized {
                import hydrozoa.multisig.ledger.l1.real.tx.RefundTx
                override type Deserialized = Tx.Refund
                override type ParseError = RefundTx.ParseError
                override def parse: Either[ParseError, Deserialized] =
                    RefundTx.parse(this)
            }

            sealed case class Settlement(
                override val headAddress: Address,
                override val headCs: CurrencySymbol,
                override val txCbor: TxCbor
            ) extends Serialized {
                import hydrozoa.multisig.ledger.l1.real.tx.SettlementTx
                override type Deserialized = Tx.Settlement
                override type ParseError = SettlementTx.ParseError
                override def parse: Either[ParseError, Deserialized] =
                    SettlementTx.parse(this)
            }

            sealed case class Rollout(
                override val headAddress: Address,
                override val headCs: CurrencySymbol,
                override val txCbor: TxCbor
            ) extends Serialized {
                import hydrozoa.multisig.ledger.l1.real.tx.RolloutTx
                override type Deserialized = Tx.Rollout
                override type ParseError = RolloutTx.ParseError
                override def parse: Either[ParseError, Deserialized] =
                    RolloutTx.parse(this)
            }

            sealed case class Fallback(
                override val headAddress: Address,
                override val headCs: CurrencySymbol,
                override val txCbor: TxCbor
            ) extends Serialized {
                import hydrozoa.multisig.ledger.l1.real.tx.FallbackTx
                override type Deserialized = Tx.Fallback
                override type ParseError = FallbackTx.ParseError
                override def parse: Either[ParseError, Deserialized] =
                    FallbackTx.parse(this)
            }

            sealed case class Finalization(
                override val headAddress: Address,
                override val headCs: CurrencySymbol,
                override val txCbor: TxCbor
            ) extends Serialized {
                import hydrozoa.multisig.ledger.l1.real.tx.FinalizationTx
                override type Deserialized = Tx.Finalization
                override type ParseError = FinalizationTx.ParseError
                override def parse: Either[ParseError, Deserialized] =
                    FinalizationTx.parse(this)
            }
        }
    }
}
