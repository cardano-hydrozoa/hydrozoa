package hydrozoa.integration.rbr.mbt

import cats.effect.{Deferred, IO, Ref}
import hydrozoa.integration.harness.MultiPeerHeadHarness
import hydrozoa.multisig.consensus.RequestSequencer

/** The running system-under-test: the multi-peer head harness plus the milestone `Deferred`s the
  * observer tracer completes as the autonomous dispute progresses.
  */
final case class Sut(
    harness: MultiPeerHeadHarness.Harness[Option[RequestSequencer.Handle]],
    fallbackDispatched: Deferred[IO, Unit],
    evacuationDone: Deferred[IO, Unit],
    firstPayoutsLeft: Ref[IO, Option[Int]],
)
