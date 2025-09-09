package hydrozoa.multisig.ledger.l1.real.tx

import com.bloxbean.cardano.client.backend.api.BackendService
import hydrozoa.multisig.ledger.l1.real.LedgerL1
import hydrozoa.multisig.ledger.l1.real.LedgerL1.{DepositUtxo, Tx}
import hydrozoa.multisig.ledger.l1.real.state.{CIP67Tags, DepositDatum}
import hydrozoa.*
import io.bullet.borer.Cbor
import scalus.builtin.Data.toData
import scalus.cardano.address.{ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionMetadatum.Bytes
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.*

import scala.util.{Failure, Success, Try}

// TDOO: Make opaque, move to a separate module?
case class DepositUtxo (utxo: Utxo[L1], datum : DepositDatum)


object DepositTx {
    case class Recipe(
        deposit: UtxoIdL1, // the only UTXO with deposit funds in user's wallet
        depositAmount: BigInt,
        datum: DepositDatum, // datum for deposit utxo
        headAddress: AddressL1,
        backendService: BackendService,
        utxoFunding: TransactionOutput
    )
    
    opaque type Tx = (Transaction, DepositUtxo)

    sealed trait ParseError
    case object HeadAddressNotFoundInMetadata extends ParseError
    case object NoUtxoAtHeadAddress extends ParseError
    
    def parse(txBytes: Tx.Serialized): Either[ParseError, Tx] = {
        given OriginalCborByteArray = OriginalCborByteArray(txBytes)
        Cbor.decode(txBytes).to[Transaction].valueTry match {
            case Success(tx) =>
            for {
                headAddress <- Tx.extractHeadAddress(tx).toRight(HeadAddressNotFoundInMetadata)
                depositUtxo <- tx.body.value.outputs.find(_.value.address == headAddress).toRight(NoUtxoAtHeadAddress)
                // TODO: Check datum is a DepositDatum and extract it
                // datum <- ??? // parseDepositDatum(depositUtxo.datumOption.get.data) (...)
            } yield (tx, DepositUtxo(utxo = ???, datum = ???))
        }
       
    }
    
    
    def build(recipe: Recipe): Either[String, Tx] = {
        // TODO: we set the fee to 1 ada, but this doesn't need to be
        val feeCoin = Coin(1_000_000)
        val depositValue: Value =
            Value(coin = Coin(recipe.depositAmount.toLong))

        val depositOutput: Babbage = Babbage(
          address = recipe.headAddress,
          value = depositValue,
          datumOption = Some(Inline(toData(recipe.datum)))
        )
        for {

            changeOutputValue <- Try(
              recipe.utxoFunding.value - Value(
                coin = feeCoin
              ) - depositValue
            ) match {
                case Success(s) => Right(s)
                case Failure(e) =>
                    Left(
                      s"Malformed value equal to `utxoFunding.value - Value(feeCoin) - depositValue`" +
                          s" encountered: utxoFunding.value = ${recipe.utxoFunding.value}, " +
                          s"depositValue = ${depositValue}, feeCoin = ${feeCoin}"
                    )
            }

            changeOutput: TransactionOutput =
                TransactionOutput(
                  address = recipe.utxoFunding.address,
                  value = changeOutputValue,
                  datumOption = None
                )

            requiredSigner: AddrKeyHash <- {
                recipe.utxoFunding.address match
                    case shelleyAddress: ShelleyAddress =>
                        shelleyAddress.payment match
                            case ShelleyPaymentPart.Key(hash) => Right(hash)
                            case _ => Left("deposit not at a pubkey address")
                    case _ => Left("Could not get key hash for required signer")
            }

            txBody = emptyTxBody.copy(
              inputs = Set(recipe.deposit),
              outputs = IndexedSeq(depositOutput, changeOutput).map(Sized(_)),
              fee = feeCoin,
              requiredSigners = Set(requiredSigner)
            )

            tx: Transaction = Transaction(
              body = KeepRaw(txBody),
              witnessSet = TransactionWitnessSet.empty,
              isValid = true,
              auxiliaryData = Some(
                Metadata(
                  Map(
                    TransactionMetadatumLabel(CIP67Tags.head) -> Bytes(recipe.headAddress.toBytes)
                  )
                )
              )
            )

            // Find the deposit output index
            ix = tx.body.value.outputs.indexWhere(output =>
                output.value.address == recipe.headAddress
            )

            _ = assert(ix >= 0, s"Deposit output was not found in the tx.")
        } yield (
          (tx, DepositUtxo(utxo = Utxo(UtxoIdL1(TransactionInput(tx.id, ix)), Output[L1](depositOutput)),
                    datum = recipe.datum)
        ))
    }
}
