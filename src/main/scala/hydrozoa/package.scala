import scalus.cardano.ledger.ProtocolVersion

// FIXME: Remove this whole file (hydrozoa/package.scala)
//  we should get the ProtocolVersion out of the head's CardanoNetwork configuration
// Package-wide givens, import using `import hydrozoa.given`
package object hydrozoa {
    given pv: ProtocolVersion = ProtocolVersion.conwayPV
}
