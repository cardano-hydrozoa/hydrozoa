package hydrozoa.rulebased.ledger.l1.script.plutus

import java.io.OutputStream

/** Runs on-chain validator code directly on the JVM without printing its traces.
  *
  * A validator's `log(...)` becomes Scalus's `trace` builtin, whose JVM implementation prints the
  * message to stdout, so a test that calls a validator as plain Scala prints one line per `log` it
  * passes. The traces only matter on-chain, where the script evaluator collects them; here the
  * outcome is what a test asserts on. `Console.withOut` applies to the calling thread, which is
  * where the validator runs.
  */
def quietTraces[A](body: => A): A =
    Console.withOut(OutputStream.nullOutputStream())(body)
