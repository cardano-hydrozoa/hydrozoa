package hydrozoa.bootstrap

import org.scalatest.funsuite.AnyFunSuite

/** Coverage for how `init-bootstrap-files` decides which chain it is targeting — the two flags that
  * name one, and what they refuse.
  *
  * Parsing only: a successful parse yields the `IO` that would write a bootstrap directory, and
  * these tests never run it. What matters here is which inputs get past the parser at all. A
  * rejection has to happen *here*, in decline, because that is what produces a usage message
  * instead of an exception from the middle of a run.
  */
class InitBootstrapFilesOptionsTest extends AnyFunSuite:

    private val roster = "roster.json"

    /** Parse an argument list, returning the failure text if decline refused it. */
    private def refusal(args: String*): Option[String] =
        InitBootstrapFiles.command.parse(args).left.toOption.map(_.toString)

    test("each standard network name is accepted") {
        List("preview", "preprod", "mainnet").foreach { name =>
            assert(
              refusal(roster, "--cardano-network", name).isEmpty,
              s"--cardano-network $name must parse"
            )
        }
    }

    test("an unknown network name is refused by the parser, naming the alternatives") {
        // Not left to `resolveChainSource`: reaching that means the run has already started, and a
        // typo surfaces as an IllegalArgumentException rather than a usage message.
        val message = refusal(roster, "--cardano-network", "preprd")
        assert(
          message.exists(m => m.contains("preprd") && m.contains("--cardano-network-file")),
          s"expected a usage error naming the typo and the escape hatch, got: $message"
        )
    }

    test("naming a chain twice is refused rather than silently resolved") {
        val message = refusal(
          roster,
          "--cardano-network",
          "preview",
          "--cardano-network-file",
          "network.json"
        )
        assert(
          message.exists(_.contains("mutually exclusive")),
          s"expected the two chain flags to conflict, got: $message"
        )
    }

    test("a chain file alone parses, and so does neither flag") {
        assert(
          refusal(roster, "--cardano-network-file", "network.json").isEmpty &&
              refusal(roster).isEmpty
        )
    }

end InitBootstrapFilesOptionsTest
