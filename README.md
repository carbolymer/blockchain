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

| Path | Description
|--|--
| `GET /healthckeck` | returns health status of the node
| `GET /chain` | returns whole blockchain
| `POST /transactions/new` | registers a new transaction
| `GET /transactions/confirmed` | lists confirmed transactions
| `GET /transactions/unconfirmed` | lists unconfirmed transactions
| `POST /mine` | mines a new block
| `POST /nodes/all` | lists all nodes known to this one
| `POST /nodes/register` | accepts a list of new nodes and add them to this one
| `POST /nodes/resolve` | queries all nodes, and checks if this one has the longest correct chain



## TODO
* [ ] Docker image
    - [ ] generating docker image from Stack
    - [ ] launching multiple nodes using Docker Compose
* [ ] Network interaction
    - [ ] automatic registration to the beacon node on start
    - [ ] resolving of nodes in network in background
    - [ ] propagation of nodes list in background
* [ ] Transactions signing with ECDSA
* [ ] Web UI
