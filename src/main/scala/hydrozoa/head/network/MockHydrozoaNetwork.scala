package hydrozoa.head.network

import hydrozoa.head.{TxKeyWitness, ParticipantVerificationKey, genNodeKey}

class MockHydrozoaNetwork extends HydrozoaNetwork {

    private val keys1 = genNodeKey()
    private val keys2 = genNodeKey()

    override def participantsKeys(): Set[ParticipantVerificationKey] =
        Set(keys1, keys2).map(_._2)

    override def reqInit(req: ReqInit): Set[TxKeyWitness] = ???
}
