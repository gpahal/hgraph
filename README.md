# hgraph

*hgraph is an in-memory graph data store inspired by [Neo4j](http://neo4j.com/) and implemented in Haskell*

## Team members

- [Garvit Pahal](https://github.com/gpahal) (12264)
- [Harshit Agrawal](https://github.com/harshitagrawal039) (12288)
- [Siddhant Saurabh](https://github.com/sidsaurb) (12715)

## Features

- Haskell embedded in-memory graph data store
- Property graph model with labels for nodes and edges
- Supported values - Int64, Bool, Double, Text, [Int64], [Bool], [Double], [Text]
- Graph algorithms - Dijkstra, Breadth first search - with support for specifying depth and number of results
- Graph algorithm results in the form of list of paths or a path tree
- Save and load the data store to disk (for backup and recovery)
- Export to Neo4j database - visualization, REST API
- Indexing based on labels
- Simple interface - findNodes, findById, dijkstra, shortestPath etc.
- Type safety as the implementation is in Haskell

## Example

A sample facebook-like social networking service implemented in [SocialNetwork.hs](https://github.com/gpahal/hgraph/blob/master/src/SocialNetwork.hs).

## Running

```bash
$ git clone https://github.com/gpahal/hgraph.git
$ cd hgraph
$ cabal run
```

## Credits

This project was started as a part of the course *CS653: Functional Programming* at IIT Kanpur under the guidance of [Prof. Piyush Kurur](https://github.com/piyush-kurur).

## License

The content of this project itself is licensed under the [BSD3](https://opensource.org/licenses/BSD-3-Clause) license. License file is located [here](https://github.com/gpahal/hgraph/blob/master/LICENSE).
