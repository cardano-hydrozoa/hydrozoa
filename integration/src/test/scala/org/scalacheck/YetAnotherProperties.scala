package org.scalacheck

import org.scalacheck.Test.Parameters
import org.scalacheck.rng.Seed
import org.scalacheck.util.ConsoleReporter
import scala.util.{Failure, Success}

abstract class YetAnotherProperties(override val name: String) extends Properties(name):

    override def main(args: Array[String]): Unit =
        CmdLineParser.parseParams(args) match {
            case (applyCmdParams, Nil) =>
                val params = applyCmdParams(overrideParameters(Test.Parameters.default))
                val res = Test.checkProperties(params, this)
                val numFailed = res.count(!_._2.passed)
                if numFailed > 0 then {
                    Console.out.println(s"Found $numFailed failing properties.")
                    System.exit(1)
                } else {
                    System.exit(0)
                }
            case (_, os) =>
                Console.out.println("Incorrect options:\n  " + os.mkString(", "))
                CmdLineParser.printHelp()
                System.exit(-1)
        }

trait CmdLineParser {

    trait Opt[+T] {
        val default: T
        val names: Set[String]
        val help: String
    }

    trait Flag extends Opt[Unit]

    trait IntOpt extends Opt[Int]

    trait FloatOpt extends Opt[Float]

    trait StrOpt extends Opt[String]

    trait OpStrOpt extends Opt[Option[String]]

    abstract class OpStrOptCompat extends OpStrOpt {
        val default: Option[String] = None
    }

    class OptMap(private val opts: Map[Opt[?], Any] = Map.empty) {
        def apply(flag: Flag): Boolean = opts.contains(flag)

        def apply[T](opt: Opt[T]): T = opts.get(opt) match {
            case None    => opt.default
            case Some(v) => v.asInstanceOf[T]
        }

        def set[T](o: (Opt[T], T)) = new OptMap(opts + o)
    }

    val opts: Set[Opt[?]]

    private def getOpt(s: String) = {
        if s == null || s.isEmpty || s.charAt(0) != '-' then None
        else opts.find(_.names.contains(s.drop(1)))
    }

    private def getStr(s: String) = Some(s)

    private def getInt(s: String) =
        if s != null && s.nonEmpty && s.forall(_.isDigit) then Some(s.toInt)
        else None

    private def getFloat(s: String) =
        if s != null && s.matches("[0987654321]+\\.?[0987654321]*") then Some(s.toFloat)
        else None

    def printHelp(): Unit = {
        Console.out.println("Available options:")
        opts.foreach { opt =>
            Console.out.println("  " + opt.names.map("-" + _).mkString(", ") + ": " + opt.help)
        }
    }

    /** Parses a command line and returns a tuple of the parsed options, and any unrecognized
      * strings
      */
    def parseArgs[T](args: Array[String]): (OptMap, List[String]) = {

        def parse(
            as: List[String],
            om: OptMap,
            us: List[String]
        ): (OptMap, List[String]) =
            as match {
                case Nil => (om, us)
                case a :: Nil =>
                    getOpt(a) match {
                        case Some(o: Flag) =>
                            parse(Nil, om.set((o, ())), us)
                        case _ =>
                            (om, us :+ a)
                    }
                case a1 :: a2 :: as =>
                    getOpt(a1) match {
                        case Some(o: Flag) =>
                            parse(a2 :: as, om.set((o, ())), us)
                        case otherwise =>
                            (otherwise match {
                                case Some(o: IntOpt) =>
                                    getInt(a2).map(v => parse(as, om.set(o -> v), us))
                                case Some(o: FloatOpt) =>
                                    getFloat(a2).map(v => parse(as, om.set(o -> v), us))
                                case Some(o: StrOpt) =>
                                    getStr(a2).map(v => parse(as, om.set(o -> v), us))
                                case Some(o: OpStrOpt) =>
                                    getStr(a2).map(v => parse(as, om.set(o -> Option(v)), us))
                                case _ => None
                            }).getOrElse(parse(a2 :: as, om, us :+ a1))
                    }
            }

        parse(args.toList, new OptMap(), Nil)
    }
}

object CmdLineParser extends CmdLineParser {
    object OptMinSuccess extends IntOpt {
        val default: Int = Parameters.default.minSuccessfulTests
        val names: Set[String] = Set("minSuccessfulTests", "s")
        val help = "Number of tests that must succeed in order to pass a property"
    }

    object OptMaxDiscardRatio extends FloatOpt {
        val default: Float = Parameters.default.maxDiscardRatio
        val names: Set[String] = Set("maxDiscardRatio", "r")
        val help: String =
            "The maximum ratio between discarded and succeeded tests " +
                "allowed before ScalaCheck stops testing a property. At " +
                "least minSuccessfulTests will always be tested, though."
    }

    object OptMinSize extends IntOpt {
        val default: Int = Parameters.default.minSize
        val names: Set[String] = Set("minSize", "n")
        val help = "Minimum data generation size"
    }

    object OptMaxSize extends IntOpt {
        val default: Int = Parameters.default.maxSize
        val names: Set[String] = Set("maxSize", "x")
        val help = "Maximum data generation size"
    }

    object OptWorkers extends IntOpt {
        val default: Int = Parameters.default.workers
        val names: Set[String] = Set("workers", "w")
        val help = "Number of threads to execute in parallel for testing"
    }

    object OptVerbosity extends IntOpt {
        val default = 1
        val names: Set[String] = Set("verbosity", "v")
        val help = "Verbosity level"
    }

    object OptPropFilter extends OpStrOptCompat {
        override val default = Parameters.default.propFilter
        val names: Set[String] = Set("propFilter", "f")
        val help = "Regular expression to filter properties on"
    }

    object OptInitialSeed extends OpStrOptCompat {
        override val default: None.type = None
        val names: Set[String] = Set("initialSeed")
        val help = "Use Base-64 seed for all properties"
    }

    object OptDisableLegacyShrinking extends Flag {
        val default: Unit = ()
        val names: Set[String] = Set("disableLegacyShrinking")
        val help = "Disable legacy shrinking using Shrink instances"
    }

    object OptMaxRNGSpins extends IntOpt {
        val default = 1
        val names: Set[String] = Set("maxRNGSpins")
        val help = "Maximum number of RNG spins to perform between checks"
    }

    val opts: Set[Opt[?]] = Set[Opt[?]](
      OptMinSuccess,
      OptMaxDiscardRatio,
      OptMinSize,
      OptMaxSize,
      OptWorkers,
      OptVerbosity,
      OptPropFilter,
      OptInitialSeed,
      OptDisableLegacyShrinking,
      OptMaxRNGSpins
    )

    def parseParams(args: Array[String]): (Parameters => Parameters, List[String]) = {
        val (optMap, us) = parseArgs(args)
        val minSuccess0: Int = optMap(OptMinSuccess)
        val minSize0: Int = optMap(OptMinSize)
        val maxSize0: Int = optMap(OptMaxSize)
        val workers0: Int = optMap(OptWorkers)
        val verbosity0 = optMap(OptVerbosity)
        val discardRatio0: Float = optMap(OptMaxDiscardRatio)
        val propFilter0: Option[String] = optMap(OptPropFilter)
        val initialSeed0: Option[Seed] =
            optMap(OptInitialSeed).flatMap { str =>
                Seed.fromBase64(str) match {
                    case Success(seed) =>
                        Some(seed)
                    case Failure(_) =>
                        println(s"WARNING: ignoring invalid Base-64 seed ($str)")
                        None
                }
            }

        val useLegacyShrinking0: Boolean = !optMap(OptDisableLegacyShrinking)
        val maxRNGSpins: Int = optMap(OptMaxRNGSpins)
        val params = { (p: Parameters) =>
            p.withMinSuccessfulTests(minSuccess0)
                .withMinSize(minSize0)
                .withMaxSize(maxSize0)
                .withWorkers(workers0)
                .withTestCallback(ConsoleReporter(verbosity0, 100000))
                .withMaxDiscardRatio(discardRatio0)
                .withPropFilter(propFilter0)
                .withInitialSeed(initialSeed0)
                .withLegacyShrinking(useLegacyShrinking0)
                .withMaxRNGSpins(maxRNGSpins)
        }
        (params, us)
    }
}
