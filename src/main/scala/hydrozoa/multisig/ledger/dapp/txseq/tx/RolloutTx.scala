package hydrozoa.multisig.ledger.dapp.txseq.tx

import hydrozoa.lib.tx.TransactionBuilder
import hydrozoa.multisig.ledger.DappLedger.Tx
import hydrozoa.multisig.ledger.dapp.utxo.RolloutUtxo
import scalus.cardano.ledger.{Transaction, Value, TransactionOutput as TxOutput}

enum RolloutTx extends Tx {
    def rolloutSpent: RolloutUtxo

    case Last(
        override val rolloutSpent: RolloutUtxo,
        override val tx: Transaction
    ) extends RolloutTx

    case Intermediate(
        override val rolloutSpent: RolloutUtxo,
        override val rolloutProduced: RolloutUtxo,
        override val tx: Transaction
    ) extends RolloutTx, RolloutTx.HasRolloutProduced

    def mbRolloutProduced: Option[RolloutUtxo] = this match {
        case x: RolloutTx.HasRolloutProduced =>
            Some(x.rolloutProduced)
        case _ => None
    }
}

object RolloutTx {
    trait HasRolloutProduced {
        def rolloutProduced: RolloutUtxo
    }

    object Builder {
        import State.Fields.*

        type Result = RolloutTx

        enum State extends HasTxBuilderContext, HasInputRequired:
            case Intermediate(
                override val txBuilderContext: TransactionBuilder.Context,
                override val inputValueRequired: Value,
                override val remainingPayoutObligations: List[TxOutput.Babbage],
                override val rolloutOutput: TxOutput.Babbage
            ) extends State, HasRemainingPayoutObligations, HasRolloutOutput

            case Last(
                override val txBuilderContext: TransactionBuilder.Context,
                override val inputValueRequired: Value,
                override val remainingPayoutObligations: List[TxOutput.Babbage]
            ) extends State, HasRemainingPayoutObligations

            case First(
                override val txBuilderContext: TransactionBuilder.Context,
                override val inputValueRequired: Value,
                override val rolloutOutput: TxOutput.Babbage
            ) extends State, HasRolloutOutput

            case Only(
                override val txBuilderContext: TransactionBuilder.Context,
                override val inputValueRequired: Value
            ) extends State

            def mbRolloutOutput: Option[TxOutput.Babbage] = this match {
                case x: HasRolloutOutput =>
                    Some(x.rolloutOutput)
                case _ => None
            }

        object State {
            type FirstOrOnly = State.First | State.Only
            type LastOrOnly = State.Last | State.Only

            object Fields {
                sealed trait HasTxBuilderContext {
                    def txBuilderContext: TransactionBuilder.Context
                }

                sealed trait HasInputRequired {
                    def inputValueRequired: Value
                }

                sealed trait HasRemainingPayoutObligations {
                    def remainingPayoutObligations: List[TxOutput.Babbage]
                }

                sealed trait HasRolloutOutput {
                    def rolloutOutput: TxOutput.Babbage
                }
            }
        }
    }
}
