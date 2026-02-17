package hydrozoa.multisig.ledger.dapp.tx

import scalus.cardano.ledger.VKeyWitness
import scalus.uplc.builtin.ByteString

type TxSignature = TxSignature.TxSignature

object TxSignature:
    opaque type TxSignature = IArray[Byte]

    def apply(signature: IArray[Byte]): TxSignature = signature

    def apply(witness: VKeyWitness): TxSignature =
        IArray.from(witness.signature.bytes)

    given Conversion[TxSignature, IArray[Byte]] = identity

    given Conversion[TxSignature, Array[Byte]] = sig => IArray.genericWrapArray(sig).toArray

    given Conversion[TxSignature, ByteString] = sig => ByteString.fromArray(sig)

    extension (signature: TxSignature) def untagged: IArray[Byte] = identity(signature)
