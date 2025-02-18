import hydrozoa.head.{HeadParams, Tx, UtxoRef}

/**
 * @param params head params
 * @param seed   seed output
 * @return serialized transaction
 */
def mkInitTx(params: HeadParams, seed: UtxoRef): Tx = {
  // TODO: implement
  Tx(Array.empty[Byte])
}
