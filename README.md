# Blockchain [![Build Status](https://travis-ci.org/carbolymer/blockchain.svg?branch=master)](https://travis-ci.org/carbolymer/blockchain)

![Screenshot of web UI](https://i.imgur.com/J39ZDdk.png)

Just a simple implementation of the blockchain, inspired by the article: https://hackernoon.com/learn-blockchains-by-building-one-117428612f46

## How to build

1. You have to have Docker and Docker Compose installed.
2. Execute following command in the project root directory to download the docker image
```
docker pull carbolymer/blockchain-node
```
3. Start the blockchain node with UI
```
docker-compose up -d ui
````
4. Open url `http://localhost:8000` in browser


## How to run

1. You have to have Docker and Docker Compose installed.
2. Execute following command in the project root directory to build the project
```
docker-compose run --rm build
```
3. Start the blockchain node
```
docker-compose up --build -d ui
````
4. Open url `http://localhost:8000` in browser


## Node HTTP Endpoints
The node is listening on `8000` port and the following endpoints are available:

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
* [ ] Web UI
    - [x] Basic UI
    - [ ] Styles
* [ ] Network interaction
    - [ ] automatic registration to the beacon node on start
    - [ ] resolving of nodes in network in background
    - [ ] propagation of nodes list in background
* [ ] Transactions signing with ECDSA
