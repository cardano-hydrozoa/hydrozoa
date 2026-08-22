package hydrozoa.rulebased.evacuator

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.joint.obligation.Payout
import hydrozoa.multisig.ledger.joint.{EvacuationKey, EvacuationMap, evacuationKeyOrdering}
import hydrozoa.rulebased.ledger.l1.script.plutus.RuleBasedTreasuryValidator.given
import scala.collection.immutable.TreeMap
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyDelegationPart, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.uplc.builtin.ByteString

/** Builds an evacuation map to test an evacuator against, without needing a head to produce one.
  *
  * A real map only exists inside a head that has run: it is the projection of L2 state at the
  * moment the head fell back. Waiting for that to exercise the bot would mean testing only against
  * whatever a live incident happens to hand us — the wrong way round, since the properties worth
  * checking (chain depth, throughput, recovery) are exactly the ones an incident does not let us
  * choose.
  *
  * What makes this safe to submit for real is that `Evacuate` is permissionless and fully
  * validated: the worst outcome of a wrong map is a transaction the ledger rejects. Funds cannot be
  * misdirected, because the validator checks value conservation and membership itself.
  *
  * Entries are deliberately the cheapest shape — an enterprise address, ada only, no datum —
  * matching what a Sugar Rush head's map is made of, so measurements here transfer to the real
  * thing.
  */
object SyntheticMap {

    /** Every payout goes back to one address we control, so the ada is recovered when the
      * evacuation completes and only fees are actually spent.
      */
    def apply(
        entries: Int,
        payTo: ShelleyPaymentPart,
        network: Network,
        perEntry: Coin
    )(using CardanoNetwork.Section): Either[Payout.Obligation.MinAdaViolation, EvacuationMap] = {
        val address = ShelleyAddress(network, payTo, ShelleyDelegationPart.Null)

        val built = (0 until entries).toList.traverseEither { i =>
            val output: TransactionOutput =
                TransactionOutput.Shelley(address, Value(perEntry), None)
            Payout
                .Obligation(KeepRaw(output), summon[CardanoNetwork.Section])
                .map(key(i) -> _)
        }

        built.map(pairs => EvacuationMap(TreeMap.from(pairs)))
    }

    /** A distinct 32-byte key per entry.
      *
      * Only distinctness and width matter: the validator treats a key as opaque bytes, and the
      * commitment is taken over key-and-output pairs. Deriving them from the index keeps a
      * generated map reproducible, so a failing run can be replayed exactly.
      */
    private def key(i: Int): EvacuationKey = {
        val bytes = Array.fill[Byte](32)(0)
        bytes(28) = ((i >>> 24) & 0xff).toByte
        bytes(29) = ((i >>> 16) & 0xff).toByte
        bytes(30) = ((i >>> 8) & 0xff).toByte
        bytes(31) = (i & 0xff).toByte
        EvacuationKey(ByteString.unsafeFromArray(bytes)).get
    }

    /** Total ada the treasury must hold to satisfy every payout in the map — what has to be locked
      * up before an evacuation can start, and the figure that decides how large a test map the
      * funding wallet can afford.
      */
    def fundingRequired(map: EvacuationMap): Coin =
        Coin(map.evacuationMap.values.map(_.utxo.value.value.coin.value).sum)

    extension [A](self: List[A])
        private def traverseEither[E, B](f: A => Either[E, B]): Either[E, List[B]] =
            self.foldRight[Either[E, List[B]]](Right(Nil)) { (a, acc) =>
                for {
                    rest <- acc
                    b <- f(a)
                } yield b :: rest
            }
}
