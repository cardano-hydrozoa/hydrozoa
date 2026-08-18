package hydrozoa.multisig.ledger.l1.tx

import cats.data.ReaderT
import hydrozoa.bootstrap.InitializationFunding
import hydrozoa.config.head.coil.{CoilPeerData, CoilPeers}
import hydrozoa.config.head.parameters.generateHeadParameters
import hydrozoa.config.head.{HeadConfig, generateHeadConfig, generateHeadConfigBootstrap}
import hydrozoa.config.node.MultiNodeConfig
import hydrozoa.multisig.consensus.peer.HeadPeerNumber
import hydrozoa.multisig.ledger.l1.txseq.InitializationTxSeq
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Gen, Prop, Properties}
import scala.collection.immutable.SortedMap
import scalus.cardano.ledger.*
import test.*
import test.given

/** Vote-token supply produced by the fallback tx, on a **coil-bearing** head.
  *
  * `InitializationTxSeqTest` already asserts the mint is `headPeersN + 1`, but every config it
  * generates has zero coil peers, where `numSigners == headPeersN` makes the assertion vacuous.
  * With `coilQuorum > 0` the two diverge, and minting `numSigners + 1` over-mints by exactly
  * `coilQuorum`: the surplus has no ballot box to go to, so the balancing diff handler sweeps it
  * into the change output — peer 0's collateral payout — which then stops being ada-only and can no
  * longer serve as the collateral the rule-based regime needs to vote.
  */
object FallbackTxVoteMintTest extends Properties("FallbackTx vote mint"):

    private val nCoilPeers = 3
    private val coilQuorum = 2

    /** Head config whose multisig carries `nCoilPeers` coil peers at `coilQuorum`, plus the funding
      * needed to build the initialization sequence it belongs to.
      */
    private val genCoilBearingHead: Gen[(HeadConfig, InitializationFunding)] =
        for {
            headPeers <- TestPeers.generate(TestPeersSpec.default)
            allPeers = TestPeers(
              headPeers.seedPhrase,
              headPeers.cardanoNetwork,
              headPeers.peersNumber + nCoilPeers
            )
            coilWallets = (0 until nCoilPeers).toList.map(i =>
                allPeers.walletFor(HeadPeerNumber(headPeers.peersNumber + i))
            )
            bootstrapAndFunding <- generateHeadConfigBootstrap(
              generateHeadParams = generateHeadParameters().map(_.copy(coilQuorum = coilQuorum)),
              coilPeers = CoilPeers.indexed(
                coilWallets.map(w => CoilPeerData(w.exportVerificationKey, HeadPeerNumber(0)))
              )
            ).run(headPeers)
            multiNodeConfig <- MultiNodeConfig.generateWith(headPeers)(
              generateHeadConfig = generateHeadConfig(
                genHeadConfigBootstrap = ReaderT.pure(bootstrapAndFunding)(using genMonad)
              )
            )
        } yield (multiNodeConfig.headConfig, bootstrapAndFunding._2)

    val _ = property("one vote token per ballot box, and peer payouts stay ada-only") =
        Prop.forAll(genCoilBearingHead) { case (config, funding) =>
            val res = InitializationTxSeq
                .Build(config, funding)(config.initialBlock.blockBrief.endTime)
                .result
            val Right(txSeq) = res: @unchecked
            val fbTxBody = txSeq.fallbackTx.tx.body.value

            val nHeadPeers = config.headPeerIds.length
            val hns = config.headMultisigScript
            val voteTokenName = config.headTokenNames.voteTokenName

            // Guard: without this the property below holds trivially on a coil-free head.
            val discriminating =
                s"expected a coil-bearing head, got coilQuorum=${config.coilQuorum}" |:
                    (config.coilQuorum > 0 && hns.numSigners > nHeadPeers)

            val mintedOnePerBox = {
                val expected = Some(
                  Mint(
                    MultiAsset(
                      SortedMap(hns.policyId -> SortedMap(voteTokenName -> (nHeadPeers + 1L)))
                    )
                  )
                )
                s"expected $expected minted, got ${fbTxBody.mint}" |: expected == fbTxBody.mint
            }

            // Output order: treasury (1), peer collateral payouts (n), peer ballot boxes (n),
            // public ballot box (1), rule-based regime (1).
            val outputs = fbTxBody.outputs.toList.map(_.value)
            val peerPayouts = outputs.slice(1, 1 + nHeadPeers)
            val ballotBoxes = outputs.slice(1 + nHeadPeers, 2 + nHeadPeers * 2)

            val payoutsAreAdaOnly =
                "peer collateral payouts must be ada-only to serve as rule-based collateral" |:
                    peerPayouts.forall(_.value.isOnlyAda)

            val everyTokenInABox = {
                val inBoxes = ballotBoxes
                    .flatMap(_.value.assets.assets.get(hns.policyId).flatMap(_.get(voteTokenName)))
                    .sum
                s"expected all ${nHeadPeers + 1} vote tokens in ballot boxes, found $inBoxes" |:
                    inBoxes == nHeadPeers + 1L
            }

            discriminating && mintedOnePerBox && payoutsAreAdaOnly && everyTokenInABox
        }
