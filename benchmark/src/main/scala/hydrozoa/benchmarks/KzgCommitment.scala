package hydrozoa.benchmarks

import hydrozoa.multisig.ledger.virtual.commitment.{KzgCommitment, TrustedSetup}
import org.openjdk.jmh.annotations.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.frequency
import org.scalacheck.{Arbitrary, Gen}
import scalus.builtin.Builtins.{blake2b_224, serialiseData}
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.*
import scalus.cardano.ledger.*
import scalus.cardano.ledger.ArbitraryInstances.{genMapOfSizeFromArbitrary, given}
import scalus.ledger.api.v3.TxInInfo
import scalus.prelude.List as SList
import scalus.|>
import supranational.blst.{P1, Scalar}

import java.util.concurrent.TimeUnit

type Utxos = UTxO

def genUtxos(size: Int) =
    genMapOfSizeFromArbitrary[TransactionInput, TransactionOutput](size, size)(using
      transactionInput,
      transactionOutput
    ).sample.get

/** N.B. don't use it BEFORE testing hashing, it will be memoized by JVM! Use it in the @TearDown
  * method after the benchmarks to control the test data set.
  */
def printUtxoStat(utxos: Utxos): Unit = {

    def toPlutus(ti: TransactionInput, to: TransactionOutput): TxInInfo =
        LedgerToPlutusTranslation.getTxInInfoV3(ti, Map(ti -> to))

    val sizes = utxos.toList
        .map(e =>
            toPlutus(e._1, e._2)
                |> (_.toData)
                |> serialiseData
                |> (_.size)
        )

    var minE, maxE, avgE: Int = sizes.head

    sizes.foreach(s =>
        minE = minE.min(s)
        maxE = maxE.max(s)
        avgE = (avgE + s) / 2
    )

    println(
      s"utxo stat: size=${utxos.size}, min utxo size=$minE, max utxo size=$maxE, average utxo size=$avgE"
    )
}

/** Rule of thumb: For serious benchmarking, put each benchmark in its own class unless the methods
  * are very closely related and share state intentionally.
  */

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Converting to Plutus ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
class UtxoToPlutusBenchmark {

    var utxos: List[(TransactionInput, TransactionOutput)] = _

    var index = 0

    def nextIndex: Int = {
        index = if index == utxos.size - 1 then 0 else index + 1
        index
    }

    @Setup(Level.Iteration)
    def setup(): Unit = utxos = genUtxos(10_000).toList

    @Benchmark
    def run() =
        val utxo = utxos(nextIndex)
        val txInInfo = LedgerToPlutusTranslation.getTxInInfoV3(utxo._1, Map(utxo._1 -> utxo._2))
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Converting to Plutus Data  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
class UtxoToDataBenchmark {

    var sampleData: List[TxInInfo] = _

    var index = 0

    def nextIndex: Int = {
        index = if index == sampleData.size - 1 then 0 else index + 1
        index
    }

    @Setup(Level.Iteration)
    def setup(): Unit = sampleData = genUtxos(10_000).toList.map(utxo =>
        LedgerToPlutusTranslation.getTxInInfoV3(utxo._1, Map(utxo._1 -> utxo._2))
    )

    @Benchmark
    def run() = sampleData(nextIndex).toData

}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Converting to Plutus Data  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
class UtxoSerializationBenchmark {

    var sampleData: List[Data] = _

    var index = 0

    def nextIndex: Int = {
        index = if index == sampleData.size - 1 then 0 else index + 1
        index
    }

    @Setup(Level.Iteration)
    def setup(): Unit = sampleData = genUtxos(10_000).toList.map(utxo =>
        LedgerToPlutusTranslation.getTxInInfoV3(utxo._1, Map(utxo._1 -> utxo._2)).toData
    )

    @Benchmark
    def run() = sampleData(nextIndex) |> serialiseData

}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Hashing Plutus Data  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
class HashByteStringBenchmark {

    var sampleData: List[ByteString] = _

    var index = 0

    def nextIndex: Int = {
        index = if index == sampleData.size - 1 then 0 else index + 1
        index
    }

    @Setup(Level.Iteration)
    def setup(): Unit = sampleData = genUtxos(10_000).toList.map(utxo =>
        LedgerToPlutusTranslation.getTxInInfoV3(utxo._1, Map(utxo._1 -> utxo._2)).toData
            |> serialiseData
    )

    @Benchmark
    def run() = sampleData(nextIndex) |> blake2b_224

}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Hashing as a whole ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@BenchmarkMode(Array(Mode.SingleShotTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class HashUtxoBenchmark {

    @Param(Array("10", "50", "100", "1000", "10000", "20000", "25000", "32767"))
    // @Param(Array("10", "50", "100"))
    var size: Int = _

    // The auto-generated input for the commitment calculation
    var utxos: Utxos = _

    @Setup(Level.Iteration)
    def setup(): Unit =
        utxos = genUtxos(size)

    @Benchmark
    def run(): Unit = KzgCommitment.hashToScalar(utxos): Unit

    @TearDown
    def stat(): Unit = printUtxoStat(utxos)
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ mkFinalPoly ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@BenchmarkMode(Array(Mode.SingleShotTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class MkFinalPolyBenchmark {

    @Param(Array("10", "50", "100", "1000", "10000", "20000", "25000", "32767"))
    // @Param(Array("10", "50", "100"))
    var size: Int = _

    var utxosHashed: SList[Scalar] = _

    @Setup(Level.Iteration)
    def setup(): Unit = {
        val utxos = genUtxos(size)
        utxosHashed = KzgCommitment.hashToScalar(utxos)
    }

    @Benchmark
    def run(): Unit = KzgCommitment.mkFinalPoly(utxosHashed): Unit

}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ evalFinalPoly ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

@BenchmarkMode(Array(Mode.SingleShotTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class EvalFinalPolyBenchmark {

    @Param(Array("10", "50", "100", "1000", "10000", "20000", "25000", "32767"))
    // @Param(Array("10", "50", "100"))
    var size: Int = _

    var finalPoly: SList[Scalar] = _
    var srs: SList[P1] = _

    @Setup(Level.Iteration)
    def setup(): Unit = {
        println(s"size: $size")
        val utxos = genUtxos(size)
        val utxosHashed = KzgCommitment.hashToScalar(utxos)
        finalPoly = KzgCommitment.mkFinalPoly(utxosHashed)
        srs = TrustedSetup.takeSrsG1(size)
    }

    @Benchmark
    def run(): Unit = KzgCommitment.evalFinalPoly(srs, finalPoly): Unit

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
