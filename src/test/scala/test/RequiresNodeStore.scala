package test

import org.scalatest.Tag

/** Tags a test that reads a real node's RocksDB store (`-Dhydrozoa.store.path`, by default
  * `~/hz-mainnet/store/rocksdb`). Such a test cancels when the store is absent. CI has no store, so
  * the build excludes this tag there (see `core` in build.sbt); local runs still run it.
  */
object RequiresNodeStore extends Tag("requires-node-store")
