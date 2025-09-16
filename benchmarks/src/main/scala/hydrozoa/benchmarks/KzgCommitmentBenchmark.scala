package hydrozoa.benchmarks

import hydrozoa.multisig.ledger.virtual.commitment.KzgCommitment
import org.openjdk.jmh.annotations.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import scalus.cardano.address.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.{genMapOfSizeFromArbitrary, given}

import java.util.concurrent.TimeUnit

type Utxos = UTxO

@BenchmarkMode(Array(Mode.SingleShotTime, Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class KzgCommitmentBenchmark {

    @Param(Array("10", "50", "100", "1000", "10000", "20000", "25000", "32767"))
    var size: Int = _

    // The auto-generated input for the commitment calculation
    var utxos: Utxos = _

    // Setup method called before each benchmark iteration
    @Setup(Level.Iteration)
    def setup(): Unit =
        utxos = genMapOfSizeFromArbitrary[TransactionInput, TransactionOutput](size, size)(using
          transactionInput,
          transactionOutput
        ).sample.get

    @Benchmark
    def run(): Unit = {
        KzgCommitment.calculateCommitment(utxos): Unit
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
        scriptRef <- arbitrary[Option[DatumOption]]
    yield TransactionOutput(address, value, scriptRef)
}
