package hydrozoa.multisig.protocol.types

object Peer {
    type Number = Number.Number

    object Number:
        opaque type Number = Int

        // TODO: require i >= 0
        def apply(i: Int): Number = i

        given Ordering[Number] with {
            override def compare(x: Number, y: Number): Int =
                x.compare(y)
        }

        given Conversion[Number, Int] = identity

        extension (self: Number) def increment: Number = Number(self + 1)
}
