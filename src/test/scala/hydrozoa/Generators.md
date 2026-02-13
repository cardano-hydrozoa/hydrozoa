# Generators

## Arbitrary generators

Generate completely random data, that can (or even SHOULD) cause errors, the name convention is:

```scala
arbitraryNetworkId: Gen[Byte]
```

## Purposed generators

This kind of generators 

### Simple generators

```scala
generateNetworkId: Gen[Byte]
```

```scala
arbitraryNetworkId: Gen[Byte]
```



Generates something respecting some bounds, the name convention is:

```scala
def generateFoo: Gen[Foo]
```


### Mandatory bounds

Should be represented as curried parameters:

```scala
generateFoo(bar: Bar): Gen[Foo]
```

Such generators cannot work as arbitrary ones, since they need some mandatory context.

### Optional bounds

Should be passed as uncurried parameters of type `... => Gen[Foo]` with mandatory defaults,
I think it's easier than using `Option`, though I didn't fully convince myself yet:

```scala

def generateFoo(
    generateBar: Gen[Bar] = arbitraryBar, 
    generateBaz: Quux => Gen[Baz] = _ => arbitraryBaz
): Gen[Foo]

```

### Combined bounds

Mandatory and optional bounds can be combined:

```scala
def generateFoo(bar: Bar)(generateBaz: Geb[Baz] = arbitraryBaz): Gen[Foo]
```

### Purposed as Arbitrary

When not having mandatory bounds, a purposed generator should have an arbitrary alias:

```scala

def generateFoo(generateBar: Gen[Bar] = arbitraryBar): Gen[Foo]

def arbitraryFoo = generateFoo()
```

The bounds can be exogenous - here it's done by knowing upfront test peers:

```scala
def generateTestPeers(minPeers: Int = 2, maxPeers: Int = 5): Gen[TestPeers] 
```
