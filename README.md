# Blockchain

Just a simple implementation of the blockchain from the article: https://hackernoon.com/learn-blockchains-by-building-one-117428612f46

## How to run

1. You have to have Haskell [Stack](https://docs.haskellstack.org/en/stable/README/) installed.
2. Execute following commands in the project root directory to build the project
```
stack setup
stack build
```
3. Start the blockchain node
```
stack exec blockchain-node-exe
````

## HTTP Endpoints
The application is listening on `http://localhost:8000/` and the following endpoints are available:
 * `GET /healthckeck` returns health status of the node
 * `POST /mine` mines a new block
 * `GET /chain` returns whole blockchain
 * `POST /nodes/register` accepts a list of new nodes
 * `POST /nodes/resolve` queries all nodes, and checks if this one has correct chain


## Known issues

* The miner holds lock over the *whole* `Blockchain` datatastructure, so it is inaccessible for other endpoints and vice versa


## TODO
* [ ] Docker container with the binary
* [ ] Registering and resolving nodes
