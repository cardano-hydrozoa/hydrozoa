import scalus.cardano.ledger.ProtocolVersion

// Package-wide givens, import using `import hydrozoa.given`
package object hydrozoa {
    given pv: ProtocolVersion = ProtocolVersion.conwayPV
}
