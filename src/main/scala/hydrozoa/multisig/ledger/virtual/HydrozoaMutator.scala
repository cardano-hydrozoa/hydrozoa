package hydrozoa.multisig.ledger.virtual
import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.lib.cardano.scalus.QuantizedTime.QuantizedInstant
import hydrozoa.multisig.ledger.VirtualLedgerM
import hydrozoa.multisig.ledger.VirtualLedgerM.{Config, State}
import hydrozoa.multisig.ledger.dapp.token.CIP67
import hydrozoa.multisig.ledger.joint.obligation.Payout
import scalus.cardano.ledger.AuxiliaryData.Metadata
import scalus.cardano.ledger.Metadatum.Int
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.cardano.ledger.rules.STS.Validator
import scalus.cardano.ledger.rules.{State as L1State, *}
import scalus.cardano.ledger.{CertState, Coin, KeepRaw, Metadatum, TransactionException, TransactionInput, TransactionOutput, Word64}

// FIXME: This is heavily inherited from the scalus STS. We don't really follow it too closely any more, so it
// could probably just be folded into VirtualLedger
object HydrozoaTransactionMutator {
    private[virtual] object CardanoLedgerContext {

        /** Turn into an L1 context with zero fee and an empty CertState
          */
        def fromCardanoNetwork(
            cardanoNetwork: CardanoNetwork.Section,
            time: QuantizedInstant
        ): Context = {
            import cardanoNetwork.*
            require(time.slotConfig == slotConfig)
            Context(
              fee = Coin(0),
              env = UtxoEnv(
                time.toSlot.slot,
                cardanoProtocolParams,
                CertState.empty,
                network
              ),
              slotConfig = slotConfig
            )
        }

    }

    def transit(
        config: Config,
        time: QuantizedInstant,
        state: State,
        l2Event: L2EventTransaction
    ): Either[String | TransactionException, State] = {
        val event = l2Event.transaction
        // A helper for mapping the error type and applying arguments
        def helper(v: Validator): Either[String | TransactionException, Unit] =
            v.validate(
              CardanoLedgerContext.fromCardanoNetwork(config, time),
              L1State(utxos = state.activeUtxos),
              event
            )
        for
            _ <- L2ConformanceValidator.validate(config, state, l2Event)
            // Upstream validators (applied alphabetically for ease of comparison in a file browser
            // FIXME/Note (Peter, 2025-07-22): I don't know if all of these will apply or if this list is exhaustive,
            // but I've removed the rules that I'm certain won't apply
            _ <- helper(AllInputsMustBeInUtxoValidator)
            _ <- helper(EmptyInputsValidator)
            _ <- helper(InputsAndReferenceInputsDisjointValidator)
            _ <- helper(MissingKeyHashesValidator)
            _ <- helper(MissingOrExtraScriptHashesValidator)
            _ <- helper(NativeScriptsValidator)
            _ <- helper(OutputsHaveNotEnoughCoinsValidator)
            _ <- helper(OutputsHaveTooBigValueStorageSizeValidator)
            _ <- helper(OutsideValidityIntervalValidator)
            _ <- helper(TransactionSizeValidator)
            _ <- helper(ValueNotConservedUTxOValidator)
            _ <- helper(VerifiedSignaturesInWitnessesValidator)
            _ <- helper(ExactSetOfRedeemersValidator)
            _ <- helper(ScriptsWellFormedValidator)
            _ <- helper(ProtocolParamsViewHashesMatchValidator)
            _ <- helper(WrongNetworkValidator)
            _ <- helper(WrongNetworkInTxBodyValidator)
            // Upstream mutators: removes inputs, adds all outputs (L1 and L2)
            scalusState <-
                PlutusScriptsTransactionMutator.transit(
                  CardanoLedgerContext.fromCardanoNetwork(config, time),
                  state.toScalusState,
                  event
                )
            // Native mutators: removes the L1-marked outputs, leaving only L2 outputs
            state <- EvacuatingMutator.transit(config, State.fromScalusState(scalusState), l2Event)
        yield state
    }
}

/** Outputs to the transaction can be marked as "L2 bound" in the transaction metadata.
  */
object EvacuatingMutator:
    final case class UtxoPartition(
        l1Utxos: IndexedSeq[(TransactionInput, Babbage)],
        l2Utxos: IndexedSeq[(TransactionInput, Babbage)]
    ) {
        def payoutObligations: Vector[Payout.Obligation] =
            Vector.from(l1Utxos.map(utxo => Payout.Obligation(utxo._1, utxo._2)))
    }

    /** @param event
      * @return
      *   An error if the metadata cant be parsed, or a pair of lists indicating (l1Bound, l2Bound)
      */
    def utxoPartition(
        event: L2EventTransaction
    ): Either[String | TransactionException, UtxoPartition] =
        for {
            metadataMap <- event.transaction.auxiliaryData match {
                case Some(keepRawM) =>
                    keepRawM.value match {
                        case Metadata(m) => Right(m)
                        case _           => Left("metadata not list")
                    }
                case _ => Left("Malformed metadata")
            }
            // Should we use a different tag here to indicate its L2?
            metaDatum <- metadataMap
                .get(Word64(CIP67.Tags.head))
                .toRight(
                  s"Head tag ${CIP67.Tags.head} not" +
                      "found in metadata map"
                )

            outputs <- {
                val outputs = event.transaction.body.value.outputs.map(_.value)
                if outputs.forall(_.isInstanceOf[Babbage])
                then Right(outputs.map(_.asInstanceOf[Babbage]))
                else Left("Non-babbage output found in utxo partition")
            }

            // TODO: This is an idiot-proof way to do it. A better way might be a bitmask -- 0 for L1, 1 for L2
            l1OrL2 <- metaDatum match {
                case Metadatum.List(il: IndexedSeq[Metadatum])
                    if il.length == outputs.length
                        && il.forall(elem => elem == Int(1) || elem == Int(2)) =>
                    Right(il)
                case _ => Left("Malformed index list in L2 transaction")
            }

            partition = {
                // NOTE/FIXME: there are multiple traversals here, but the transformation is a little bit
                // tricky. This can be refactored to do it in one pass if it becomes a bottleneck.

                // Format: (output, l1OrL2, index)
                val zippedOutputs =
                    outputs.zip(l1OrL2).zipWithIndex.map(x => (x._1._1, x._1._2, x._2))

                // format: ((input, output), l1orL2)
                val utxosWithDesignation = zippedOutputs.map(x =>
                    ((TransactionInput(event.transaction.id, x._3), x._1), x._2)
                )

                // format: ([((l1Input, l1Output), l1orL2)] , [((l2Input, l2Output), l1orL2)])
                val partitionWithDesignation =
                    utxosWithDesignation.partition(x => if x._2 == Int(1) then true else false)

                UtxoPartition(
                  partitionWithDesignation._1.map(_._1),
                  partitionWithDesignation._2.map(_._1)
                )
            }

        } yield partition

    def transit(
        config: Config,
        state: State,
        event: L2EventTransaction
    ): Either[String | TransactionException, State] =
        for {
            p <- utxoPartition(event)
            l1UtxosToRemove = p.l1Utxos.map(_._1).toSet
        } yield state.copy(state.activeUtxos -- l1UtxosToRemove)
