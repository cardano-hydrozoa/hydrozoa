package hydrozoa.multisig.ledger.dapp.txseq

import cats.data.NonEmptyList
import hydrozoa.multisig.ledger.dapp.script.multisig.HeadMultisigScript
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.dapp.tx.{Metadata as _, *}
import hydrozoa.multisig.ledger.dapp.txseq.InitializationTxSeq.Builder.Error.InitializationTxError
import hydrozoa.multisig.ledger.dapp.utxo.MultisigTreasuryUtxo
import hydrozoa.rulebased.ledger.dapp.script.plutus.DisputeResolutionScript
import hydrozoa.rulebased.ledger.dapp.state.VoteDatum as VD
import hydrozoa.{VerificationKeyBytes, ensureMinAda, maxNonPlutusTxFee, given}
import scala.collection.immutable.SortedMap
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.cardano.address.*
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.ledger.*
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.txbuilder.*
import scalus.cardano.txbuilder.TransactionBuilder.ResolvedUtxos
import scalus.ledger.api.v1.{PosixTime, PubKeyHash}

final case class InitializationTxSeq(initializationTx: InitializationTx, fallbackTx: FallbackTx)

object InitializationTxSeq {

    sealed trait ParseError
    case class InitializationTxParseError(wrapped: InitializationTx.ParseError) extends ParseError
    case class FallbackTxBuildError(wrapped: SomeBuildError) extends ParseError
    case class FallbackTxMismatch(expected: FallbackTx, actual: Transaction) extends ParseError

    /** Given two transaction that should form a valid Initialization-Fallback Transaction Sequence,
      * we:
      *   - Parse the first transaction as an initialization transaction
      *   - Use the result to build a fallback transaction
      *   - Compare the second transaction given to the constructed fallback transaction. If they
      *     don't match exactly, we error.
      *
      * Note that the parsing of the initialization transaction isn't currently guaranteed to be
      * secure. We are parsing primarily to ensure that the given transaction won't result in a head
      * that will immediately crash.
      *
      * @param transactionSequence
      * @param expectedNetwork
      * @param peerKeys
      * @param expectedTallyFeeAllowance
      * @param expectedVotingDuration
      * @param env
      * @param evaluator
      * @param validators
      * @param resolver
      * @return
      */
    def parse(
        transactionSequence: (Transaction, Transaction),
        expectedNetwork: Network,
        peerKeys: NonEmptyList[VerificationKeyBytes],
        expectedTallyFeeAllowance: Coin,
        expectedVotingDuration: PosixTime,
        env: Environment,
        evaluator: PlutusScriptEvaluator,
        validators: Seq[Validator],
        resolver: Seq[TransactionInput] => ResolvedUtxos
    ): Either[ParseError, InitializationTxSeq] = {

        val initializationTx = transactionSequence._1

        for {
            iTx <- InitializationTx
                .parse(
                  peerKeys,
                  expectedNetwork = expectedNetwork,
                  tx = initializationTx,
                  resolver = resolver
                )
                .left
                .map(InitializationTxParseError(_))

            config = Tx.Builder.Config(
              headNativeScript = HeadMultisigScript(peerKeys),
              multisigRegimeUtxo = iTx.multisigRegimeWitness,
              tokenNames = iTx.tokenNames,
              env = env,
              evaluator = evaluator,
              validators = validators
            )
            ftxRecipe = FallbackTx.Recipe(
              config = config,
              treasuryUtxoSpent = iTx.treasuryProduced,
              tallyFeeAllowance = expectedTallyFeeAllowance,
              votingDuration = expectedVotingDuration
            )
            expectedFallbackTx <- FallbackTx.build(ftxRecipe).left.map(FallbackTxBuildError(_))
            _ <-
                if expectedFallbackTx.tx == transactionSequence._2 then Right(())
                else
                    Left(
                      FallbackTxMismatch(
                        expected = expectedFallbackTx,
                        actual = transactionSequence._2
                      )
                    )

        } yield InitializationTxSeq(initializationTx = iTx, fallbackTx = expectedFallbackTx)
    }

    /* OUTLINE:
         The HMRW utxo is of a fixed size -- it has no datum and a fixed amount of tokens.
         However, those tokens depend on the size of the vote utxos and collateral utxos that are in the fallback
         transaction.

         Thus, there is a pseudo-dependency for building the initialization transaction -- we want to know the
         size of the outputs of the fallback tx, but we don't actually need to construct the full fallback transaction.
         We only need to construct its transaction outputs.

         The two options I thought of for doing this was:

         - (1) Make a fallback "partial result" that returns the ada amount needed for the HRMW utxo and a callback to
           finish the transaction.
         - (2) Duplicate a little bit of work and construct the utxos in this builder as well as in the fallback tx
           builder.

          I settled on (2) because it was the quickest to get running, while simplifying the fallback tx builder and
           allowing it to be reused in the settlement tx sequence builder with identical semantics (just pass in the
           treasury).
     */
    object Builder {

        def build(args: Args): Either[Error, InitializationTxSeq] = {
            val tokenNames = CIP67.TokenNames(args.spentUtxos.seedUtxo.input)
            val disputeResolutionAddress = ShelleyAddress(
              network = args.env.network,
              payment = ShelleyPaymentPart.Script(DisputeResolutionScript.compiledScriptHash),
              delegation = Null
            )

            import tokenNames.*

            // ===================================
            // Head Native Script
            // ===================================

            // Construct head native script directly from the list of peers
            val hns = HeadMultisigScript(args.peers)

            // ===================================
            // Init Treasury
            // ===================================
            val initTreasuryDatum = MultisigTreasuryUtxo.mkInitMultisigTreasuryDatum

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
              address = disputeResolutionAddress,
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
              evaluator = args.evaluator,
              validators = args.validators,
              changePP = args.initializationTxChangePP
            )

            for {
                initializationTx <- InitializationTx
                    .build(initializationTxRecipe)
                    .left
                    .map(Error.InitializationTxError(_))

                config = Tx.Builder.Config(
                  headNativeScript = hns,
                  multisigRegimeUtxo = initializationTx.multisigRegimeWitness,
                  tokenNames = initializationTx.tokenNames,
                  env = args.env,
                  evaluator = args.evaluator,
                  validators = args.validators
                )

                fallbackTxRecipe = FallbackTx.Recipe(
                  config = config,
                  treasuryUtxoSpent = initializationTx.treasuryProduced,
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
            env: Environment,
            evaluator: PlutusScriptEvaluator,
            validators: Seq[Validator],
            initializationTxChangePP: ShelleyPaymentPart,
            tallyFeeAllowance: Coin,
            votingDuration: PosixTime
        )
    }
}
