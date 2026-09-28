package hydrozoa.app.cli

import org.scalatest.funsuite.AnyFunSuite

/** Coverage for `discover-network`'s all-or-nothing geometry flags.
  *
  * Parsing only: a successful parse yields the `IO` that would query a backend, and these tests
  * never run it. What matters is which combinations get past the parser — a half-told chain has to
  * be refused with a usage message, not accepted and silently completed from somewhere else.
  */
class DiscoverNetworkOptionsTest extends AnyFunSuite:

    private val url = List("--blockfrost-url", "http://localhost:18080/api/v1")

    private val geometry = List(
      "--system-start",
      "1785935807",
      "--slot-length",
      "1.0",
      "--epoch-length",
      "600",
      "--protocol-magic",
      "42"
    )

    /** Parse an argument list, returning the failure text if decline refused it. */
    private def refusal(args: List[String]): Option[String] =
        DiscoverNetwork.command.parse(args).left.toOption.map(_.toString)

    test("the backend is asked for the geometry when none is given") {
        assert(refusal(url).isEmpty)
    }

    test("a complete geometry parses, fractional slot length included") {
        assert(
          refusal(url ++ geometry).isEmpty &&
              refusal(url ++ geometry.patch(3, List("0.5"), 1)).isEmpty
        )
    }

    test("a partly told geometry is refused, naming all four flags") {
        // Every proper subset must fail: dropping one flag pair at a time covers the realistic
        // slips, and the message has to say what the complete set is.
        val subsets = geometry.grouped(2).toList
        val messages = subsets.indices.map { dropped =>
            refusal(url ++ subsets.patch(dropped, Nil, 1).flatten)
        }
        assert(
          messages.forall(m =>
              m.exists(text =>
                  List("--system-start", "--slot-length", "--epoch-length", "--protocol-magic")
                      .forall(text.contains)
              )
          ),
          s"every incomplete geometry must be refused with the full set named; got $messages"
        )
    }

end DiscoverNetworkOptionsTest
