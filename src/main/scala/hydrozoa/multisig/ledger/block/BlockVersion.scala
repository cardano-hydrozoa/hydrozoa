package hydrozoa.multisig.ledger.block

import cats.implicits.catsSyntaxOrder

object BlockVersion {
    type Full = Full.Full
    type Major = Major.Major
    type Minor = Minor.Minor

    object Full {
        opaque type Full = (Int, Int)

        def apply(i: Int, j: Int): Full = (i, j)

        val zero: Full = (0, 0)

        def unapply(self: Full): (Major, Minor) = (Major(self._1), Minor(self._2))

        given Conversion[Full, (Int, Int)] = identity

        given Ordering[Full] with {
            override def compare(x: Full, y: Full): Int =
                x.compare(y)
        }

        extension (self: Full)
            def major: Major = Major(self._1)
            def minor: Minor = Minor(self._2)
            def incrementMajor: Full = Full(self._1 + 1, 0)
            def incrementMinor: Full = Full(self._1, self._2 + 1)
    }

    object Major {
        opaque type Major = Int

        def apply(i: Int): Major =
            require(i >= 0)
            i

        val zero: Major = 0

        given Conversion[Major, Int] = identity

        given Ordering[Major] with {
            override def compare(x: Major, y: Major): Int =
                x.compare(y)
        }

        extension (self: Major)
            def increment: Major = Major(self + 1)
            def decrement: Major =
                if self == zero
                then throw RuntimeException("Attempt of version major decrement on 0")
                else Major(self - 1)

        trait Produced {
            def majorVersionProduced: Major
        }
    }

    object Minor {
        opaque type Minor = Int

        def apply(i: Int): Minor =
            require(i >= 0)
            i

        val zero: Minor = 0

        given Conversion[Minor, Int] = identity

        given Ordering[Minor] with {
            override def compare(x: Minor, y: Minor): Int =
                x.compare(y)
        }

        extension (self: Minor) def increment: Minor = Minor(self + 1)
    }
}
