package hydrozoa.multisig.ledger.commitment

import hydrozoa.config.head.network.CardanoNetwork
import hydrozoa.multisig.ledger.block.BlockNumber
import hydrozoa.multisig.ledger.joint.EvacuationMap as JointEvacuationMap
import hydrozoa.multisig.ledger.stack.{PartitionEffects, Stack, StackEffects, StackNumber}
import hydrozoa.multisig.persistence.StoreKey
import java.nio.ByteBuffer
import java.nio.file.{Files, Path, Paths}
import java.util.ArrayList as JArrayList
import org.rocksdb.{ColumnFamilyDescriptor, ColumnFamilyHandle, DBOptions, Options, RocksDB}
import org.scalatest.funsuite.AnyFunSuite
import scala.jdk.CollectionConverters.*

/** Does the current derivation reproduce the commitments a real head already signed?
  *
  * The sibling `KzgCommitmentStoreFidelityTest` checks the fast derivation against the schoolbook
  * one over the same inputs. That is an equivalence property: it cannot detect a commitment that
  * both implementations compute the same way and the deployed binary computed differently, which is
  * the failure that would fork a live head or strand funds behind an unspendable treasury.
  *
  * This closes that gap from the other side. Every standalone evacuation commitment in the store
  * carries the block it commits to and the commitment value the head signed at the time — a value
  * produced by whatever binary was running then, not by this one. Re-deriving from the stored map
  * at that block and comparing is a conformance check against recorded output rather than against a
  * second implementation.
  *
  * Skipped when no synced store is present, so it is a no-op in CI.
  */
class KzgCommitmentOnChainConformanceTest extends AnyFunSuite {

    private val storePath: Path = Paths.get(
      sys.props.getOrElse(
        "hydrozoa.store.path",
        s"${sys.props("user.home")}/hz-mainnet/store/rocksdb"
      )
    )

    private given CardanoNetwork.Section with {
        def cardanoNetwork: CardanoNetwork = CardanoNetwork.Mainnet
    }

    test("every commitment a real head signed is reproduced by the current derivation") {
        val _ = assume(Files.isDirectory(storePath), s"no store at $storePath")
        RocksDB.loadLibrary()

        val names = RocksDB.listColumnFamilies(new Options(), storePath.toString).asScala.toList
        val descriptors = new JArrayList[ColumnFamilyDescriptor]()
        names.foreach(n => descriptors.add(new ColumnFamilyDescriptor(n)))
        val handles = new JArrayList[ColumnFamilyHandle]()
        val db = RocksDB.openReadOnly(new DBOptions(), storePath.toString, descriptors, handles)

        try {
            def handleFor(cf: String): ColumnFamilyHandle =
                names
                    .zip(handles.asScala)
                    .collectFirst {
                        case (n, h) if new String(n, "UTF-8") == cf => h
                    }
                    .get

            // Codecs are declared on the store keys, so go through them — this is then the same
            // decode the recovery path performs, not a re-derivation that could drift from it.
            val mapCodec = StoreKey.EvacuationMap(BlockNumber.zero).codec
            val stackCodec = StoreKey.UnsignedStack(StackNumber.zero).codec

            // Pass 1: the cumulative map at each block, by block number (4-byte big-endian key).
            val mapsByBlock = scala.collection.mutable.HashMap.empty[Int, JointEvacuationMap]
            val mapIt = db.newIterator(handleFor("EvacuationMap"))
            try {
                mapIt.seekToFirst()
                while mapIt.isValid do {
                    val blockNum = ByteBuffer.wrap(mapIt.key()).getInt
                    val _ = mapsByBlock.put(blockNum, mapCodec.decode(mapIt.value()))
                    mapIt.next()
                }
            } finally mapIt.close()

            // Pass 2: every commitment the head actually signed, with the block it commits to.
            var checked = 0
            var disagreed = 0
            var noStoredMap = 0
            var stacks = 0
            val firstDisagreement = scala.collection.mutable.ListBuffer.empty[String]

            val stackIt = db.newIterator(handleFor("UnsignedStack"))
            try {
                stackIt.seekToFirst()
                while stackIt.isValid do {
                    val stack: Stack.Unsigned = stackCodec.decode(stackIt.value())
                    stacks += 1
                    val partitions = stack.effects match {
                        case r: StackEffects.Unsigned.Regular => r.partitions.toList
                        case _: StackEffects.Unsigned.Initial => Nil // stack 0 carries no SEC
                    }
                    partitions.foreach { p =>
                        val secs = p match {
                            case PartitionEffects.Major(_, _, _, _, sec) => sec.toList
                            case PartitionEffects.Minor(sec, _)          => List(sec)
                            case _: PartitionEffects.Final               => Nil
                        }
                        secs.foreach { sec =>
                            mapsByBlock.get(sec.blockNum: Int) match {
                                case None => noStoredMap += 1
                                case Some(map) =>
                                    checked += 1
                                    if map.kzgCommitment != sec.kzgCommitment then {
                                        disagreed += 1
                                        if firstDisagreement.size < 3 then
                                            firstDisagreement += s"block=${sec.blockNum: Int} " +
                                                s"signed=${sec.kzgCommitment} " +
                                                s"derived=${map.kzgCommitment}"
                                    }
                            }
                        }
                    }
                    stackIt.next()
                }
            } finally stackIt.close()

            println(
              s"[kzg-conformance] store=$storePath stacks=$stacks maps=${mapsByBlock.size} " +
                  s"commitmentsChecked=$checked disagreed=$disagreed noStoredMap=$noStoredMap"
            )
            // The control: with no commitments checked the equality assertion below is vacuous.
            val _ = assert(checked > 0, "no signed commitments could be paired with a stored map")
            assert(
              disagreed == 0,
              s"$disagreed of $checked signed commitments were not reproduced: " +
                  firstDisagreement.mkString("; ")
            )
        } finally db.close()
    }
}
