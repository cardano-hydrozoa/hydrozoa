# Hydrozoa Demo Setup

This folder contains a demo-suite for Hydrozoa.
It allows to spin up a test Hydrozoa peer network of three nodes:
Alice, Bob, and Carol along with a Cardano L1 private testnet
powered by [Yaci DevKit](https://github.com/bloxbean/yaci-devkit/tree/main)
and auxiliary containers for tracing, collection and showing metrics -
Prometheus, Grafana, and others.

You can play with the Hydrozoa network manually by using client APIs
exposed on all its three peers or by using a demo workload which generates
a barrage of user commands to see how it works under some load.

## Prerequisites

1. Nix manager to enter dev-shell (or JVM, Scala, and `sbt` of proper versions).
2. Docker and `docker-compose`, rootless mode is supported.

## How to run

1. Build the Docker image of Hydrozoa node.
   The image is not yet publicly available, but you can easily build it locally
   using `sbt` tool from project's root folder:

```bash
[in Nix shell, project root]
$ sbt docker:publishLocal
```

Check the image is in the local registry:

```bash
$ docker images | grep hydrozoa
cardano-hydrozoa/hydrozoa   0.1.0-SNAPSHOT   3ee4e908c85c   45 years ago    574MB
```

2. Spin up containers using `docker-compose` (or `docker compose`) from `demo` folder:

```bash
$ cd demo
$ docker compose up
```

Once up, the following should work:

* Yaci Viewer - http://localhost:5173/
* Grafana - http://localhost:3003/

Navigate to provisioned Hydrozoa dashboard to observe the head.

## Cleaning up

In some cases you might want to remove containers and volumes
(be careful, this command can DESTROY your data):

```bash
docker system prune --volumes
```

## Running demo workload

To run a sample demo workload run the following command from project's root folder:

```bash
sbt demo/run
```

## ...or play with a Hydrozoa head manually

Follow Alice's logs in a separate window:

```bash
$ docker logs -f hydrozoa-alice
```

You can use Postman collection located in the `demo` folder or `curl`:

Run an initialization request:

```bash
curl --location --request PUT 'localhost:8093/init?amount=100&txId=6d36c0e2f304a5c27b85b3f04e95fc015566d35aef5f061c17c70e3e8b9ee508&txIx=0'
```

Run a deposit request:

```bash
curl --location --request PUT 'localhost:8093/deposit?txId=39174fac6bab286ec46e3ffc156b6c59b9bf2c85a8e24164f8cf99c8e13e78b0&txIx=1&depositAmount=42000000&address=addr_test1qryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana68ca5aqa6swlq6llfamln09tal7n5kvt4275ckwedpt4v7q48uhex&datum=d8799f400040ff&refundAddress=addr_test1qryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana68ca5aqa6swlq6llfamln09tal7n5kvt4275ckwedpt4v7q48uhex&refundDatum=d8799f400040ff'
```
