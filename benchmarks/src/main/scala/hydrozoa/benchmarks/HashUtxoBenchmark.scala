package hydrozoa.benchmarks

import hydrozoa.multisig.ledger.virtual.commitment.{KzgCommitment, TrustedSetup}
import org.openjdk.jmh.annotations.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.frequency
import org.scalacheck.{Arbitrary, Gen}
import scalus.cardano.address.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.{genMapOfSizeFromArbitrary, given}
import scalus.prelude.List as SList
import supranational.blst.{P1, Scalar}

import java.util.concurrent.TimeUnit

type Utxos = UTxO

/** Rule of thumb: For serious benchmarking, put each benchmark in its own class unless the methods
  * are very closely related and share state intentionally.
  */

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Hashing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@BenchmarkMode(Array(Mode.SingleShotTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class HashUtxoBenchmark {

    @Param(Array("10", "50", "100", "1000", "10000", "20000", "25000", "32767"))
    var size: Int = _

    // The auto-generated input for the commitment calculation
    var utxos: Utxos = _

    @Setup(Level.Iteration)
    def setup(): Unit = {
        println(s"size: $size")
        utxos = genMapOfSizeFromArbitrary[TransactionInput, TransactionOutput](size, size)(using
          transactionInput,
          transactionOutput
        ).sample.get
    }

    @Benchmark
    def run(): Unit = {
        KzgCommitment.hashToScalar(utxos): Unit
    }
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ mkFinalPoly ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@BenchmarkMode(Array(Mode.SingleShotTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class MkFinalPolyBenchmark {

    @Param(Array("10", "50", "100", "1000", "10000", "20000", "25000", "32767"))
    var size: Int = _

    var utxosHashed: SList[Scalar] = _

    @Setup(Level.Iteration)
    def setup(): Unit = {
        println(s"size: $size")
        val utxos = genMapOfSizeFromArbitrary[TransactionInput, TransactionOutput](size, size)(using
          transactionInput,
          transactionOutput
        ).sample.get
        utxosHashed = KzgCommitment.hashToScalar(utxos)
    }

    @Benchmark
    def run(): Unit = {
        KzgCommitment.mkFinalPoly(utxosHashed): Unit
    }
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ evalFinalPoly ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@BenchmarkMode(Array(Mode.SingleShotTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class EvalFinalPolyBenchmark {

    @Param(Array("10", "50", "100", "1000", "10000", "20000", "25000", "32767"))
    var size: Int = _

    var finalPoly: SList[Scalar] = _
    var srs: SList[P1] = _

    @Setup(Level.Iteration)
    def setup(): Unit = {
        println(s"size: $size")
        val utxos = genMapOfSizeFromArbitrary[TransactionInput, TransactionOutput](size, size)(using
          transactionInput,
          transactionOutput
        ).sample.get
        val utxosHashed = KzgCommitment.hashToScalar(utxos)
        finalPoly = KzgCommitment.mkFinalPoly(utxosHashed)
        srs = TrustedSetup.takeSrsG1(size)
    }

    @Benchmark
    def run(): Unit = {
        KzgCommitment.evaluateFinalPoly(srs, finalPoly): Unit
    }

}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Custom Arbitrary instances ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

given enterpriseShelleyAddress: Arbitrary[Address] = Arbitrary {
    for
        network <- Arbitrary.arbitrary[Network]
        paymentPart <- Arbitrary.arbitrary[ShelleyPaymentPart]
    yield ShelleyAddress(network, paymentPart, ShelleyDelegationPart.Null)
}

given transactionInput: Arbitrary[TransactionInput] = Arbitrary {
    for
        transactionId <- arbitrary[TransactionHash]
        index <- Gen.choose(0, Int.MaxValue)
    yield TransactionInput(transactionId, index)
}

given transactionOutput: Arbitrary[TransactionOutput] = Arbitrary {
    for
        address <- arbitrary[Address](using enterpriseShelleyAddress)
        value <- arbitrary[Value]
        mbDatum <- frequency(
          5 -> arbitrary[Option[DatumOption]],
          95 -> Gen.const(None)
        )
    yield TransactionOutput(address, value, mbDatum)
}
