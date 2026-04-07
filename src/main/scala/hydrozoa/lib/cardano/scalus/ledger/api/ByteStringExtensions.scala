package hydrozoa.lib.cardano.scalus.cardano.onchain.plutus

import scalus.*
import scalus.uplc.builtin.Builtins.*
import scalus.uplc.builtin.{Builtins, ByteString}

@Compile
object ByteStringExtension:
    extension (self: ByteString)
        inline infix def <(other: ByteString): Boolean =
            lessThanByteString(self, other)

        inline infix def <=(other: ByteString): Boolean = {
            lessThanEqualsByteString(self, other)
        }

        inline infix def >(other: ByteString): Boolean =
            lessThanByteString(other, self)

        inline infix def >=(other: ByteString): Boolean =
            lessThanEqualsByteString(other, self)

        inline def at(index: BigInt): BigInt = indexByteString(self, index)

        // TODO: If macros allow for custom compile time warnings, would be nice
        //   to warn user: "use `at` when is called with arg n := 1".
        inline def take(n: BigInt): ByteString = sliceByteString(BigInt(0), n, self)

        inline def slice(from: BigInt, len: BigInt): ByteString =
            sliceByteString(from, len, self)

        inline def drop(n: BigInt): ByteString = {
            val rest = lengthOfByteString(self) - n
            sliceByteString(n, rest, self)
        }
