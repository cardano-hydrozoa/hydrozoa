package hydrozoa.multisig

import hydrozoa.lib.logging.Level
import hydrozoa.multisig.HeadMultisigRegimeManager.Actors
import hydrozoa.multisig.consensus.peer.{CoilPeerNumber, HeadPeerNumber}
import org.scalatest.funsuite.AnyFunSuite

/** A regime manager stops its own multisig children at the handoff to the rule-based regime, so
  * their terminations then are expected (INFO); any other child termination is a warning.
  */
class TerminatedActorLevelTest extends AnyFunSuite {

    private def headLevel(atHandoff: Boolean): Level =
        HeadMultisigRegimeManagerEventFormat
            .humanFormat(HeadPeerNumber.zero)(
              LifecycleEvent.TerminatedActor(Actors.BlockWeaver, atHandoff)
            )
            .level

    private def coilLevel(atHandoff: Boolean): Level =
        CoilMultisigRegimeManagerEventFormat
            .humanFormat(HeadPeerNumber.zero, CoilPeerNumber.zero)(
              LifecycleEvent.TerminatedActor(Actors.BlockWeaver, atHandoff)
            )
            .level

    test("a head's child terminated at the handoff is INFO, otherwise WARN") {
        val _ = assert(headLevel(atHandoff = true) == Level.Info)
        assert(headLevel(atHandoff = false) == Level.Warn)
    }

    test("a coil's child terminated at the handoff is INFO, otherwise WARN") {
        val _ = assert(coilLevel(atHandoff = true) == Level.Info)
        assert(coilLevel(atHandoff = false) == Level.Warn)
    }
}
