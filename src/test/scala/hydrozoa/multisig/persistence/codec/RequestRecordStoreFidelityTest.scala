package hydrozoa.multisig.persistence.codec

import java.nio.file.{Files, Path, Paths}
import java.util.ArrayList as JArrayList
import org.rocksdb.{ColumnFamilyDescriptor, ColumnFamilyHandle, DBOptions, Options, RocksDB}
import org.scalatest.funsuite.AnyFunSuite
import scala.jdk.CollectionConverters.*

/** Does [[RequestRecordCodec]] carry every request a real head has actually held?
  *
  * The golden fixtures pin four hand-chosen cases. This runs the same round trip over every record
  * in a synced production store — hundreds of thousands of them, at the sizes and shapes real
  * traffic produced rather than the ones someone thought to write down.
  *
  * `encode(decode(stored)) == stored` is the property. It is stronger than "decodes without
  * throwing": a codec that silently dropped a field would still decode, and would still be wrong
  * for anything that re-encodes what it read.
  *
  * The store is an external fixture, so this **cancels** rather than fails when it is absent — a
  * clone without one is not a broken build. Point it elsewhere with
  * `-Dhydrozoa.store.path=/some/rocksdb`.
  */
class RequestRecordStoreFidelityTest extends AnyFunSuite {

    private val storePath: Path =
        Paths.get(
          sys.props.getOrElse(
            "hydrozoa.store.path",
            s"${sys.props("user.home")}/hz-mainnet/store/rocksdb"
          )
        )

    /** The persistence layer's generic journal framing: 12 bytes of arrival stamp, then the record.
      * Skipped here for the same reason a Rust reader skips it — it carries no protocol meaning.
      */
    private val stampBytes = 12

    test("every Request record in a real store survives a decode/encode round trip") {
        assume(Files.isDirectory(storePath), s"no store at $storePath")
        RocksDB.loadLibrary()

        val names = RocksDB
            .listColumnFamilies(new Options(), storePath.toString)
            .asScala
            .toList
        // `Request:<peer>` — the per-author journal. Not `RequestHighWater` or `RequestBlockIndex`,
        // which share the prefix and hold something else entirely; a looser filter reads those as
        // malformed records and reports a codec failure that is really a test bug.
        val requestCfs = names.filter(n => new String(n, "UTF-8").startsWith("Request:"))
        assume(requestCfs.nonEmpty, s"no Request column family in $storePath")

        val descriptors = new JArrayList[ColumnFamilyDescriptor]()
        names.foreach(n => descriptors.add(new ColumnFamilyDescriptor(n)))
        val handles = new JArrayList[ColumnFamilyHandle]()
        val db = RocksDB.openReadOnly(new DBOptions(), storePath.toString, descriptors, handles)

        try {
            val byName = names.map(n => new String(n, "UTF-8")).zip(handles.asScala).toMap
            var checked = 0
            var mismatched = 0
            var undecodable = 0
            val examples = scala.collection.mutable.ListBuffer.empty[String]

            requestCfs.foreach { cfName =>
                val handle = byName(new String(cfName, "UTF-8"))
                val it = db.newIterator(handle)
                try {
                    it.seekToFirst()
                    while it.isValid do {
                        val stored = it.value()
                        if stored.length > stampBytes then {
                            val payload = stored.drop(stampBytes)
                            scala.util.Try(
                              RequestRecordCodec.encode(RequestRecordCodec.decode(payload))
                            ) match {
                                case scala.util.Success(reencoded) =>
                                    if !java.util.Arrays.equals(reencoded, payload) then {
                                        mismatched += 1
                                        if examples.size < 5 then
                                            examples += s"key=${it.key().mkString(",")} " +
                                                s"stored=${payload.length}B re-encoded=${reencoded.length}B"
                                    }
                                case scala.util.Failure(e) =>
                                    undecodable += 1
                                    if examples.size < 5 then
                                        examples += s"undecodable: ${e.getMessage}"
                            }
                            checked += 1
                        }
                        it.next()
                    }
                } finally it.close()
            }

            println(
              s"[fidelity] store=$storePath cfs=${requestCfs.length} checked=$checked " +
                  s"mismatched=$mismatched undecodable=$undecodable"
            )
            // The control: a run that checked nothing would pass every assertion below.
            assert(checked > 0, s"no Request records found in $storePath")
            assert(
              mismatched == 0 && undecodable == 0,
              s"$mismatched of $checked records re-encoded differently and $undecodable did not " +
                  s"decode: ${examples.mkString("; ")}"
            )
        } finally {
            handles.asScala.foreach(_.close())
            db.close()
        }
    }
}
