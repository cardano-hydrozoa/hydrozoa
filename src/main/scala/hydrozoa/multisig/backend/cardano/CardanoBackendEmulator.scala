package hydrozoa.multisig.backend.cardano

import cats.effect.{IO, Ref}
import scalus.cardano.ledger.*
import scalus.cardano.node.Emulator
import scalus.uplc.builtin.Data

class EmulatorContinuingTxTracker private (
    submittedTxsRef: Ref[IO, List[Transaction]],
    emulator: Emulator
) extends ContinuingTxTracker[IO]:

    def recordTx(tx: Transaction): IO[Unit] =
        submittedTxsRef.update(_ :+ tx)

    override def lastContinuingTxs(
        asset: (PolicyId, AssetName),
        after: TransactionHash
    ): IO[Either[CardanoBackend.Error, List[(TransactionHash, Data)]]] =
        submittedTxsRef.get.map { submittedTxs =>
            val txsReversed = submittedTxs.reverse
            val txsAfter = txsReversed.takeWhile(_.id != after)
            val result = txsAfter.flatMap { tx =>
                continuingInputRedeemer(tx, asset, emulator.utxos).map(tx.id -> _)
            }
            Right(result)
        }

    private def continuingInputRedeemer(
        tx: Transaction,
        asset: (PolicyId, AssetName),
        utxos: Utxos
    ): Option[Data] =
        val inputWithAssetIdx = tx.body.value.inputs.toSeq.zipWithIndex
            .find { (input, _) =>
                utxos.get(input).exists(_.value.hasAsset(asset._1, asset._2))
            }
            .map(_._2)

        val hasOutputWithAsset =
            tx.body.value.outputs.exists(_.value.value.hasAsset(asset._1, asset._2))

        for
            inputIx <- inputWithAssetIdx
            _ <- Option.when(hasOutputWithAsset)(())
            redeemers <- tx.witnessSet.redeemers.map(_.value)
            redeemer <- redeemers.toSeq.find(r =>
                r.tag == RedeemerTag.Spend && r.index.toInt == inputIx
            )
        yield redeemer.data

object EmulatorContinuingTxTracker:
    def apply(emulator: Emulator): IO[EmulatorContinuingTxTracker] =
        Ref.of[IO, List[Transaction]](List.empty).map { ref =>
            new EmulatorContinuingTxTracker(ref, emulator)
        }


object CardanoBackendEmulator:
    def apply(emulator: Emulator): IO[CardanoBackendV2] =
        EmulatorContinuingTxTracker(emulator).map { tracker =>
            val backend = new CardanoBackendV2(emulator, tracker) {
                override def submitTx(
                    tx: Transaction
                ): IO[Either[CardanoBackend.Error, Unit]] =
                    super.submitTx(tx).flatTap {
                        case Right(_) => tracker.recordTx(tx)
                        case Left(_)  => IO.unit
                    }
            }
            backend
        }
