package test

/** Seed phrases are used as a way to conveniently derive multiple keys for test peers. You can swap
  * them to change the keys and, accordingly, addresses a head and peers use. There are a default
  * phrase that originally comes from Yaci, which is used by default (even for tests that are not
  * integrational and therefore don't require Yaci).
  *
  * NB: If I get it right, Yaci doesn't allow topping up arbitrary addresses (?). At the same time I
  * don't want to use Yaci's seed phrase on public testnets, since likely we will clash with other
  * devs.
  */
enum SeedPhrase:

    /** Default for non-integration tests and integration tests on Yaci. */
    case Yaci

    /** Default for integration tests on a public testnet. */
    case Public

    /** In case you want to use something else. */
    case Custom(_mnemonic: String)

    def mnemonic: String = this match {
        case SeedPhrase.Yaci =>
            "test test test test " +
                "test test test test " +
                "test test test test " +
                "test test test test " +
                "test test test test " +
                "test test test sauce"

        case SeedPhrase.Public =>
            "frog initial risk tail " +
                "either august present asset " +
                "muffin glass fat olympic " +
                "secret thunder envelope "

        case Custom(mnemonic) => mnemonic
    }
