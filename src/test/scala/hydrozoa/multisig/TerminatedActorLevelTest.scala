package hydrozoa.multisig

import hydrozoa.lib.logging.Level
import hydrozoa.multisig.HeadMultisigRegimeManager.Actors
import hydrozoa.multisig.LifecycleEvent.Stopping
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber}
import org.scalatest.funsuite.AnyFunSuite

/** A regime manager stops its own children at the handoff to the rule-based regime and when the
  * node shuts down, so their terminations then are expected (INFO); any other child termination is
  * a warning.
  */
class TerminatedActorLevelTest extends AnyFunSuite {

    private def headLevel(stopping: Option[Stopping]): Level =
        HeadMultisigRegimeManagerEventFormat
            .humanFormat(HeadPeerNumber.zero)(
              LifecycleEvent.TerminatedActor(Actors.BlockWeaver, stopping)
            )
            .level

    private def coilLevel(stopping: Option[Stopping]): Level =
        CoilMultisigRegimeManagerEventFormat
            .humanFormat(HeadPeerNumber.zero, CoilPeerNumber.zero)(
              LifecycleEvent.TerminatedActor(Actors.BlockWeaver, stopping)
            )
            .level

    test("a head's child terminated at the handoff or at shutdown is INFO, otherwise WARN") {
        val _ = assert(headLevel(Some(Stopping.AtHandoff)) == Level.Info)
        val _ = assert(headLevel(Some(Stopping.AtShutdown)) == Level.Info)
        assert(headLevel(None) == Level.Warn)
    }

    test("a coil's child terminated at the handoff or at shutdown is INFO, otherwise WARN") {
        val _ = assert(coilLevel(Some(Stopping.AtHandoff)) == Level.Info)
        val _ = assert(coilLevel(Some(Stopping.AtShutdown)) == Level.Info)
        assert(coilLevel(None) == Level.Warn)
    }
}
