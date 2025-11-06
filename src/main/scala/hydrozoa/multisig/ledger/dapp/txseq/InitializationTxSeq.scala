package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.tx.*
import hydrozoa.multisig.ledger.dapp.txseq.InitializationTxSeq.Builder.Error.InitializationTxError
import hydrozoa.multisig.ledger.dapp.utxo.TreasuryUtxo
import hydrozoa.rulebased.ledger.dapp.state.VoteDatum as VD
import hydrozoa.{VerificationKeyBytes, ensureMinAda, maxNonPlutusTxFee}
import scalus.builtin.Data
import scalus.builtin.ToData.toData
import scalus.cardano.address.*
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.txbuilder.*
import scalus.ledger.api.v1.{PosixTime, PubKeyHash}

import scala.collection.immutable.SortedMap

final case class InitializationTxSeq(initializationTx: InitializationTx, fallbackTx: FallbackTx)

object InitializationTxSeq {
    object Builder {
        /* OUTLINE:
        The HMRW utxo is of a fixed size -- it has no datum and a fixed amount of tokens.
        However, those tokens depend on the size of the vote utxos and collateral utxos that are in the fallback
        transaction.

        Thus, there is a pseudo-dependency for building the initialization transaction -- we want to know the
        size of the outputs of the fallback tx, but we don't actually need to construct the full fallback transaction.
        We only need to construct it's transaction outputs.

        The two options I thought of for doing this was:

        - (1) Make a fallback "partial result" that returns the ada amount needed for the HRMW utxo and a callback to
          finish the transaction.
        - (2) Duplicate a little bit of work and construct the utxos in this builder as well as in the fallback tx
          builder.

         I settled on (2) because it was the quickest to get running, while simplifying the fallback tx builder and
          allowing it to be reused in the settlement tx sequence builder with identical semantics (just pass in the
          treasury).

         */
        def build(args: Args): Either[Error, InitializationTxSeq] = {
            val tokenNames = CIP67.TokenNames(args.spentUtxos.seedUtxo.input)
            import tokenNames.*

            // ===================================
            // Head Native Script
            // ===================================

            // Construct head native script directly from the list of peers
            val hns = HeadMultisigScript(args.peers)

            // singleton beacon token minted by the native script with the TN being the hash of the
            // seed utxos
            val headToken: MultiAsset = MultiAsset(
              SortedMap(
                hns.script.scriptHash -> SortedMap(headTokenName -> 1L)
              )
            )

            val headAddress: ShelleyAddress = hns.mkAddress(args.env.network)
            // Head output (L1) sits at the head address with the initial deposit from the seed utxo
            // and beacon, as well as the initial datum.
            val headValue: Value =
                Value(coin = args.initialDeposit, assets = headToken)

            // ===================================
            // Init Treasury
            // ===================================
            val initTreasuryDatum = TreasuryUtxo.mkInitMultisigTreasuryDatum

            val initialTreasuryUtxo: Babbage = Babbage(
              address = headAddress,
              value = headValue,
              datumOption = Some(Inline(initTreasuryDatum.toData)),
              scriptRef = None
            )

            // ===================================
            // Vote Utxos
            // ===================================
            def mkVoteToken(amount: Long): MultiAsset = MultiAsset(
              SortedMap(
                (
                  hns.policyId,
                  SortedMap((voteTokenName, amount))
                )
              )
            )

            def mkVoteUtxo(datum: Data): TransactionOutput = Babbage(
              address = args.disputeResolutionAddress,
              value = Value(args.tallyFeeAllowance, mkVoteToken(1)),
              datumOption = Some(Inline(datum)),
              scriptRef = None
            ).ensureMinAda(args.env.protocolParams)

            val initDefaultVoteUtxo: TransactionOutput = mkVoteUtxo(
              VD.default(initTreasuryDatum.commit).toData
            )

            val peerVoteUtxos: NonEmptyList[TransactionOutput] = {
                val datums = VD(
                  NonEmptyList.fromListUnsafe(
                    hns.requiredSigners.map(x => PubKeyHash(x.hash)).toList
                  )
                )
                datums.map(datum => mkVoteUtxo(datum.toData))
            }

            // ===================================
            // Collateral utxos
            // ===================================

            def collateralUtxos: NonEmptyList[TransactionOutput] = {
                NonEmptyList.fromListUnsafe(
                  hns.requiredSigners
                      .map(es =>
                          Babbage(
                            address = ShelleyAddress(
                              network = args.env.network,
                              payment = ShelleyPaymentPart.Key(es.hash),
                              delegation = Null
                            ),
                            value = Value(args.tallyFeeAllowance),
                            datumOption = None,
                            scriptRef = None
                          ).ensureMinAda(args.env.protocolParams)
                      )
                      .toList
                )
            }

            // ===================================
            // Init Tx
            // ===================================

            val multisigRegimeWitnessCoin: Coin =
                Coin(
                  maxNonPlutusTxFee(args.env.protocolParams).value
                      + initDefaultVoteUtxo.value.coin.value
                      + peerVoteUtxos.map(_.value.coin.value).toList.sum
                      + collateralUtxos.map(_.value.coin.value).toList.sum
                )

            val initializationTxRecipe = InitializationTx.Recipe(
              spentUtxos = args.spentUtxos,
              headNativeScript = hns,
              initialDeposit = args.initialDeposit,
              tokenNames = tokenNames,
              hmrwCoin = multisigRegimeWitnessCoin,
              env = args.env,
              validators = args.validators,
              changePP = args.initializationTxChangePP
            )

            for {
                initializationTx <- InitializationTx
                    .build(initializationTxRecipe)
                    .left
                    .map(Error.InitializationTxError(_))

                fallbackTxRecipe = FallbackTx.Recipe(
                  config = initializationTx.resultingConfig,
                  treasuryUtxo = initializationTx.treasuryProduced,
                  disputeTreasuryPaymentPart = args.disputeTreasuryPaymentPart,
                  disputeResolutionPaymentPart = args.disputeResolutionPaymentPart,
                  tallyFeeAllowance = args.tallyFeeAllowance,
                  votingDuration = args.votingDuration
                )

                fallbackTx <- FallbackTx
                    .build(fallbackTxRecipe)
                    .left
                    .map(Error.FallbackTxError(_))

            } yield InitializationTxSeq(initializationTx, fallbackTx)
        }

        // TODO: Make the individual builders actually throw these (typed) errors
        enum Error {
            case FallbackPRError(e: SomeBuildError)
            case InitializationTxError(e: SomeBuildError)
            case FallbackTxError(e: SomeBuildError)
        }

        final case class Args(
            spentUtxos: InitializationTx.SpentUtxos,
            initialDeposit: Coin,
            peers: NonEmptyList[VerificationKeyBytes],
            disputeTreasuryPaymentPart: ShelleyPaymentPart,
            disputeResolutionPaymentPart: ShelleyPaymentPart,
            env: Environment,
            validators: Seq[Validator],
            initializationTxChangePP: ShelleyPaymentPart,
            tallyFeeAllowance: Coin,
            // FIXME: Unused
            votingDuration: PosixTime
        ) {

            val disputeTreasuryAddress = ShelleyAddress(
              network = env.network,
              payment = disputeTreasuryPaymentPart,
              delegation = Null
            )

            val disputeResolutionAddress = ShelleyAddress(
              network = env.network,
              payment = disputeResolutionPaymentPart,
              delegation = Null
            )
        }
    }
}
