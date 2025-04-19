# Hydrozoa Demo Setup

This folder contains a demo-suite for Hydrozoa.
It allows to spin up a test Hydrozoa peer network  of three nodes: 
Alice, Bob, and Carol along with a Cardano L1 private testnet
powered by [Yaci DevKit](https://github.com/bloxbean/yaci-devkit/tree/main) 
and auxiliary containers for tracing, collection and showing metrics - 
Prometheus, Grafana, and others.

You can play with the Hydrozoa network manually by using client APIs
exposed on all its three peers or by using a test-suite to generate
a barrage of user commands to see how it works under some load.

## Prerequisites

1. Nix manager to enter dev-shell (or JVM, Scala, and `sbt` of proper versions).
2. Docker and `docker-compose`, rootless mode is supported.

## How to run

1. Build the image of Hydrozoa node. 
The image is not yet publicly available, but you can easily build it locally
using `sbt` tool:

```bash
[in Nix shell, project root]
$ sbt docker:publishLocal
$ docker images | grep hydrozoa
cardano-hydrozoa/hydrozoa   0.1.0-SNAPSHOT   3ee4e908c85c   45 years ago    574MB
```

2. Spin up containers using `docker-compose`

```bash
$ cd demo
$ docker-compose up
```

3. Run Yaci (this manual step should be gone)

```bash
$ docker exec -it yaci-cli bash
root@yaci-cli: /app# ./yaci-cli.sh
devnet:default> create-node -o
...
devnet:default> start
...
```

After that the following things should work:
1. Yaci Viewer - http://localhost:5173/ - should show slots and genesis txs.
2. Prometheus - http://localhost:9090/targets - all targets including Alice, Bob, and Carol should be green.
3. Grafana - http://localhost:3003/ (not yet configured properly)

4. Play with Hydrozoa:

Follow Alice's logs in a separate window:

```bash
$ docker logs -f hydrozoa-alice
```

Run an initialization request:

```bash
curl --location --request PUT 'localhost:8093/init?amount=100&txId=6d36c0e2f304a5c27b85b3f04e95fc015566d35aef5f061c17c70e3e8b9ee508&txIx=0'
```

Run a deposit request:

```bash
curl --location --request PUT 'localhost:8093/deposit?txId=39174fac6bab286ec46e3ffc156b6c59b9bf2c85a8e24164f8cf99c8e13e78b0&txIx=1&address=addr_test1qryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana68ca5aqa6swlq6llfamln09tal7n5kvt4275ckwedpt4v7q48uhex&datum=d8799f400040ff&refundAddress=addr_test1qryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana68ca5aqa6swlq6llfamln09tal7n5kvt4275ckwedpt4v7q48uhex&refundDatum=d8799f400040ff'
```

## Running test-suite

TBD
