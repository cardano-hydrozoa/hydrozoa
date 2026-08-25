package hydrozoa.multisig.ledger.commitment

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.joint.EvacuationMap as JointEvacuationMap
import hydrozoa.multisig.persistence.StoreKey
import java.nio.file.{Files, Path, Paths}
import java.util.ArrayList as JArrayList
import org.rocksdb.{ColumnFamilyDescriptor, ColumnFamilyHandle, DBOptions, Options, RocksDB}
import org.scalatest.funsuite.AnyFunSuite
import scala.jdk.CollectionConverters.*

/** Does the accumulator library agree with the textbook formulation on the maps a real head has
  * actually committed to?
  *
  * [[KzgCommitmentTest]] pins the two against each other over generated sets and at the sizes that
  * straddle the library's algorithm switches. This runs the same equality over every evacuation map
  * in a synced production store — the exact shapes mainnet committed to, in the order it committed
  * to them.
  *
  * The commitment is a consensus value: it goes into the treasury datum every peer signs and into
  * the on-chain evacuation proof. A disagreement on any one of these is a fork, not a slowdown.
  *
  * ⚠️ This covers **correctness on real shapes, not the large-N regime**. Mainnet's maps are small,
  * and the run prints their size distribution so the claim is bounded by what was actually tested.
  * The large-N evidence is the demo-stand sweep in the PR.
  *
  * External fixture, so it cancels rather than fails when absent. Point it elsewhere with
  * `-Dhydrozoa.store.path=/some/rocksdb`.
  */
class KzgCommitmentStoreFidelityTest extends AnyFunSuite {

    private val storePath: Path = Paths.get(
      sys.props.getOrElse(
        "hydrozoa.store.path",
        s"${sys.props("user.home")}/hz-mainnet/store/rocksdb"
      )
    )

    private given CardanoNetwork.Section with {
        def cardanoNetwork: CardanoNetwork = CardanoNetwork.Mainnet
    }

    test("every evacuation map in a real store commits to the same point both ways") {
        assume(Files.isDirectory(storePath), s"no store at $storePath")
        RocksDB.loadLibrary()

        val names = RocksDB.listColumnFamilies(new Options(), storePath.toString).asScala.toList
        val target = names.find(n => new String(n, "UTF-8") == "EvacuationMap")
        assume(target.isDefined, s"no EvacuationMap column family in $storePath")

        val descriptors = new JArrayList[ColumnFamilyDescriptor]()
        names.foreach(n => descriptors.add(new ColumnFamilyDescriptor(n)))
        val handles = new JArrayList[ColumnFamilyHandle]()
        val db = RocksDB.openReadOnly(new DBOptions(), storePath.toString, descriptors, handles)

        try {
            val handle = names
                .zip(handles.asScala)
                .collectFirst {
                    case (n, h) if new String(n, "UTF-8") == "EvacuationMap" => h
                }
                .get
            // The codec is declared on the key, so go through the key rather than re-deriving it —
            // this is then the same decode the recovery path performs.
            val codec = StoreKey.EvacuationMap(BlockNumber.zero).codec

            var checked = 0
            var disagreed = 0
            val sizes = scala.collection.mutable.SortedMap.empty[Int, Int]
            val firstDisagreement = scala.collection.mutable.ListBuffer.empty[String]

            val it = db.newIterator(handle)
            try {
                it.seekToFirst()
                while it.isValid do {
                    val map: JointEvacuationMap = codec.decode(it.value())
                    val scalars = map.scalars
                    val n = scalars.length.toInt
                    sizes.updateWith(n)(c => Some(c.getOrElse(0) + 1))
                    if KzgCommitment.calculateKzgCommitment(scalars) != Schoolbook.commitment(
                          scalars
                        )
                    then {
                        disagreed += 1
                        if firstDisagreement.size < 3 then
                            firstDisagreement += s"blockKey=${it.key().mkString(",")} setSize=$n"
                    }
                    checked += 1
                    it.next()
                }
            } finally it.close()

            println(
              s"[kzg-fidelity] store=$storePath maps=$checked disagreed=$disagreed " +
                  s"setSizes=${sizes.map((n, c) => s"$n:$c").mkString(" ")}"
            )
            // The control: a run that decoded nothing would satisfy the equality assertion below.
            assert(checked > 0, s"no evacuation maps found in $storePath")
            assert(
              disagreed == 0,
              s"$disagreed of $checked maps committed to a different point: " +
                  firstDisagreement.mkString("; ")
            )
        } finally {
            handles.asScala.foreach(_.close())
            db.close()
        }
    }
}
