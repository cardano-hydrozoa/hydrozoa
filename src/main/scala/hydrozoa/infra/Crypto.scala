package hydrozoa.infra

import com.bloxbean.cardano.client.crypto.Blake2bUtil.{blake2bHash224, blake2bHash256}

// Opaque types and functions for cryptographic hashes used in Hydrozoa.

object CryptoHash:

    // H28 - Blake2b224
    opaque type H28[A] = IArray[Byte]
    // H32 - Blake2b256
    opaque type H32[A] = IArray[Byte]

    object H28 {
        def hash[A](input: IArray[Byte]): H28[A] =
            apply(IArray.from[Byte](blake2bHash224(IArray.genericWrapArray(input).toArray)))

        private def apply[A](bytes: IArray[Byte]): H28[A] =
            require(bytes.length == 28, "Blake2b224 hash must be exactly 28 bytes long")
            bytes

        // TODO: Remove once we migrate to IArray
        def hash_[A](input: Array[Byte]): H28[A] =
            apply(IArray.from[Byte](blake2bHash224(input)))

        extension [A](x: H28[A]) {
            def bytes: IArray[Byte] = x
        }
    }

    object H32 {
        def hash[A](input: IArray[Byte]): H32[A] =
            apply(IArray.from[Byte](blake2bHash256(IArray.genericWrapArray(input).toArray)))

        private def apply[A](bytes: IArray[Byte]): H32[A] =
            require(bytes.length == 32, "Blake2b256 hash must be exactly 32 bytes long")
            bytes

        extension [A](x: H32[A]) {
            def bytes: IArray[Byte] = x
        }
    }
