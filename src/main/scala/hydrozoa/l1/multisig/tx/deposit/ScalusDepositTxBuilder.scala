package hydrozoa.l1.multisig.tx.deposit

import com.bloxbean.cardano.client.backend.api.BackendService
import hydrozoa.infra.transitionary.{bloxToScalusUtxoQuery, emptyTxBody, toScalus}
import hydrozoa.node.state.{HeadStateReader, multisigRegime}
import hydrozoa.{Tx, TxIx, TxL1}
import io.bullet.borer.Cbor
import scalus.builtin.Data.toData
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyPaymentPart, StakePayload}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.txbuilder.{BuilderContext, TxBuilder}

class ScalusDepositTxBuilder(
    backendService: BackendService,
    reader: HeadStateReader,
    builderContext: BuilderContext
) extends DepositTxBuilder {

    override def buildDepositTxDraft(recipe: DepositTxRecipe): Either[String, (TxL1, TxIx)] = {

        bloxToScalusUtxoQuery(backendService, recipe.deposit.toScalus) match {
            case Left(err) => Left(s"Scalus DepositTxBuilder failed: ${err}")
            case Right(utxoFunding) =>
                Right({
                    val depositValue: Value =
                        Value(coin = Coin(recipe.depositAmount.toLong))
                    val headAddress: Address =
                        Address.fromBech32(reader.multisigRegime(_.headBechAddress).bech32)

                    val tx = builderContext
                        .withUtxo(Map(recipe.deposit.toScalus -> utxoFunding))
                        .buildNewTx
                        .payToAddress(headAddress, depositValue, toData(recipe.datum))
                        .doFinalize

                    // Find the deposit outout index
                    val ix = tx.body.value.outputs.indexWhere(output =>
                        output.value.address == headAddress
                    )

                    assert(ix >= 0, s"Deposit output was not found in the tx.")

                    (Tx(Cbor.encode(tx).toByteArray), TxIx(ix))
                })

        }
    }
}
