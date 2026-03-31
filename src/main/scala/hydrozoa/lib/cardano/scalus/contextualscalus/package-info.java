package hydrozoa.lib.cardano.scalus.contextualscalus;

/**
 * This package is experimenting with writing a layer on top of scalus that assumes you're building a 
 * sane, concrete dApp rather than needing full generic parameters.
 * 
 * Particularly, its gonig to assume that you have things like a `Network` or `ProtocolParams` in scope in a
 * `using` where ever its practical.
 */