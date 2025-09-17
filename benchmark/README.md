# Hydrozoa benchmarks

Here we put benchmarks for important parts that are likely to become bottlenecks.
Also, we are using this document as a dumping ground for ideas that may help us
improve the performance.

## How to run

We use the JMH plugin for `sbt`:

```
sbt:core> benchmark / Jmh / run -i 5 -wi 5 -f1 -t1
```

## KZG commitment

Efficient calculation of KZG commitment is of utter importance since every _minor_
block requires a new commitment.

The Hydrozoa MVP targets small L2 virtual ledgers of 1k-3k utxos, and for now, we are using
the naive and simplest approach that we will evolve in the upcoming projects.
We use the trusted setup for _powers of tau_ from [Ethereum Summoning Ceremony](https://audit.kzgceremony.eth.limo/),
which allows us to support up to 32k utxos (once we have quick enough algorithms).

A KZG commitment is a point in the G1 subgroup of the BLS curve. 
To calculate it, one needs to:
1. Take all utxos in the L2
2. Convert them to the Plutus representation (this is needed so the treasury validator can verify membership proofs onchain)
3. Convert to Plutus `Data`
4. Serialize as `ByteString`
5. Calculate the `blake2b_224`
6. Make scalars from the hash values
7. Build the final polynomial $f[x]$ calculating its coefficients
8. Evaluate the $g_1^{f(\tau)}$ using the trusted setup

The naive approach is to rebuild the commitment for every block from scratch.

### Current results

Steps 2-5 are measured by multiple runs, these are the average values in _microseconds_: 

```
Benchmark                           (size)  Mode  Cnt    Score   Error  Units
(2) UtxoToPlutusBenchmark              N/A  avgt        53.397          us/op
(3) UtxoToDataBenchmark                N/A  avgt       320.622          us/op
(4) UtxoSerializationBenchmark         N/A  avgt       287.210          us/op
(5) HashByteStringBenchmark            N/A  avgt       389.961          us/op
```

The whole naive approach for different-sized sets of utxo, _milliseconds_ by 
running them 5 times:
          
Hashing (steps 2-6):

```
Benchmark                  (size)  Mode  Cnt    Score   Error  Units   
HashUtxoBenchmark           10    ss          256.854          ms/op
HashUtxoBenchmark           50    ss          326.072          ms/op
HashUtxoBenchmark          100    ss          342.577          ms/op
HashUtxoBenchmark         1000    ss          464.346          ms/op
HashUtxoBenchmark        10000    ss         1205.174          ms/op
HashUtxoBenchmark        20000    ss         1390.332          ms/op
HashUtxoBenchmark        25000    ss         1697.658          ms/op
HashUtxoBenchmark        32767    ss         1803.217          ms/op
```

Making the final polynomial, step (7) - the bottleneck for big sets:
                                               
```
Benchmark                  (size)  Mode  Cnt    Score   Error  Units   
MkFinalPolyBenchmark        10    ss            2.312          ms/op
MkFinalPolyBenchmark        50    ss            5.907          ms/op
MkFinalPolyBenchmark       100    ss            6.157          ms/op
MkFinalPolyBenchmark      1000    ss          260.102          ms/op
MkFinalPolyBenchmark     10000    ss        21045.077          ms/op
MkFinalPolyBenchmark     20000    ss        84924.395          ms/op
MkFinalPolyBenchmark     25000    ss       132028.882          ms/op
MkFinalPolyBenchmark     32767    ss       217247.202          ms/op
```

Evaluating the polynomial, step (8):

```
Benchmark                  (size)  Mode  Cnt    Score   Error  Units   
EvalFinalPolyBenchmark      10    ss            6.262          ms/op
EvalFinalPolyBenchmark      50    ss           10.135          ms/op
EvalFinalPolyBenchmark     100    ss           20.161          ms/op
EvalFinalPolyBenchmark    1000    ss          178.678          ms/op
EvalFinalPolyBenchmark   10000    ss         1070.223          ms/op
EvalFinalPolyBenchmark   20000    ss         2546.657          ms/op
EvalFinalPolyBenchmark   25000    ss         2471.935          ms/op
EvalFinalPolyBenchmark   32767    ss         4256.330          ms/op
```

### Implementation plan

For sizes we are targeting within the scope of the MVP, 
the bottleneck is _hashing_ (steps 2-5). This can be easily fixed by calculating them upfront 
during the initial L2 transaction handling. The L2 ledger is to store those hashes so 
they can act as the inputs to the naive procedure.
This measure will allow us to calculate the commitment in **500ms** for **1000** utxos.

If we need to improve this quickly, we can:
* Utilize the _divide-and-conquer_ method to improve step (6), still $O(N^2)$ but with better constant.
* Parallelize the step (6).
* Leverage `blst`'s multi-scalar multiplication at step (7), which should give us 25x at 1000 elements.

### Future work

For bigger utxo sets, starting from a _couple of thousands_, 
we need _an incremental approach_ for calculating the commitment.
This may come in different flavors (at the same time).

### Hot-spot optimization

This depends on the particular use case, but it's known that usually 
most changes in the ledger occur in some relatively small hot spot, 
while the rest stand still. Very likely, newer utxos have a higher probability of being spent
than older ones. Hydrozoa's consensus allows the total order of L2 transactions.
It means we can easily sort utxos according to their age and split them up into
generations and/or regions in a way that GCs work. This measure  will 
opportunistically speed up the calculation by reusing commitments for
unchanged parts of the ledger.

### Diff-based algorithm

Every block removes some utxos and adds others. 
From the commitment calculation perspective, we could use the polynomial for previous 
block $f_{n-1}[x]$ for:
* _removing_ spent utxos by _dividing_ it by the corresponding binomials
* _adding_ newly created utxos by _multiplying_ it by the corresponding binomials

While the latter can be done easily, the former is complicated and proved to be slow
if done in the _coefficient representation_. However, this should be relatively easy in
the _point-value_ representation.
Since BLS12-381 was designed specifically for discrete FFT/NTT transformation, this seems to be a viable option.
In that case:

* Upon the first genesis event, when the head is just created, 
  we build the final polynomial directly and turn it into
  point-value representation form using NTT, let's say in block $n-1$.
* For every next block $n$ we build two partial polynomials for $f^{Added}_n[x]$ and $f^{Gone}_n[x]$
  and convert them into point-value representation based on the degree of the polynomial for the previous block $n-1$. 
* Now we can use point-wise multiplication and division to build the final polynomial for block $n$.
* To build the commitment, we need to run _inverse FFT_ to get the coefficients.
* Finally, step (8) can be done using MSM.

NTT should be doable in $O(n * \log n)$, which would be a substantial improvement.

Some useful links:

* https://www.nayuki.io/page/number-theoretic-transform-integer-dft?utm_source=chatgpt.com - 
  light introduction to NTT and some Java code.
* https://github.com/wendykierp/JTransforms/tree/master - a library that supports different transformations, 
  but probably not those we need.
* https://flintlib.org/doc/flint.pdf - this is based on GMP and uses DFT/NTT under the hood,
  though it's not clear whether it's exposed enough, and this is a C library.
* https://cips.cardano.org/cip/CIP-0133 - see MSM benchmarks here.
* https://baincapitalcrypto.com/a-deep-dive-into-logjumps-a-faster-modular-reduction-algorithm/ - 
  relatively new approach for modular reduction
