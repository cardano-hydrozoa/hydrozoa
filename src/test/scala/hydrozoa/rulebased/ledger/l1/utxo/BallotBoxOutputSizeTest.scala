package hydrozoa.rulebased.ledger.l1.utxo

import hydrozoa.config.head.multisig.fallback.FallbackContingency
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.lib.number.PositiveInt
import hydrozoa.rulebased.ledger.l1.state.VoteState.VoteStatus
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.utils.MinCoinSizedTransactionOutput
import scalus.cardano.ledger.{Coin, Sized, TransactionOutput}
import scalus.uplc.builtin.ByteString
import test.TestPeersSpec

/** Measures the CBOR size and minAda of a `Voted` ballot-box output, validating
  * `FallbackContingency.Assumptions.maxBallotBoxBytes` — the assumed size that fixes the ballot
  * box's minAda, and hence `publicVoteDeposit` / `forBallotBox`, at which the Open-phase box is
  * funded with **no headroom**. If a real `Voted` output exceeds the assumed size, its true minAda
  * is higher than the box is funded for, so a ratchet cannot reproduce the box's value and the
  * DisputeResolution "continuing vote output with the same value" check fails.
  */
class BallotBoxOutputSizeTest extends AnyFunSuite:

    test("Voted ballot-box output fits maxBallotBoxBytes and is funded to its minAda"):
        val env = MultiNodeConfig
            .generate(TestPeersSpec.default)()
            .pureApply(Gen.Parameters.default, Seed(0L))
        given config: BallotBoxConfig = env.nodeConfigs.head._2

        // The public Open-phase box (key=0, link=1) ratcheted forward: a 48-byte compressed-G1 KZG
        // commitment plus a versionMinor. This is the box that ratchets on-chain.
        val voted = VoteStatus.Voted(
          commitment = ByteString.fromArray(Array.fill(48)(0xff.toByte)),
          versionMinor = BigInt("18446744073709551615") // u64 max — worst-case CBOR width
        )
        val output: TransactionOutput = BallotBoxOutput(
          key = BigInt(0),
          link = BigInt(1),
          coin = config.collectiveContingency.publicVoteDeposit,
          voteTokens = PositiveInt.unsafeApply(1),
          status = voted
        ).toOutput

        val sized = Sized(output)
        val minAda = MinCoinSizedTransactionOutput.ensureMinAda(sized, config.cardanoProtocolParams)
        val bound: Int = FallbackContingency.Assumptions.maxBallotBoxBytes
        val funded: Coin = config.collectiveContingency.publicVoteDeposit
        println(
          f"voted ballot box  cborBytes=${sized.size}%4d  bound=$bound%4d  " +
              f"minAda=${minAda.value}%9d  funded=${funded.value}%9d lovelace"
        )

        val _ = assert(
          sized.size <= bound,
          s"Voted ballot-box output measured ${sized.size} CBOR bytes; exceeds maxBallotBoxBytes $bound"
        )
        assert(
          minAda.value <= funded.value,
          s"Voted ballot-box minAda ${minAda.value} exceeds funded publicVoteDeposit ${funded.value}"
        )
