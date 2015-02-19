# ZDDs + tile-based algorithmic self-assembly

This program enumerates the set of paths between two points in a lattice. 

Users can choose whether they want for this path set to consist of all paths or only Hamiltonian paths.

Additionally users can determine whether a given temperature 1 tile set can be assembled such that
every vertex along the path is covered. This assembly must be well-formed, that is, the tile edges
along the path must match. 

At the heart of the calculations I'm utilizing a data structure known as a zero-supressed decision diagram (ZDD).

## Establish Parameters

![set parameters](https://cloud.githubusercontent.com/assets/6858645/6261376/e1a4286e-b7b3-11e4-8fdb-91c677db9b1a.JPG)

## Paths

![simple path](https://cloud.githubusercontent.com/assets/6858645/6261396/07b1a0cc-b7b4-11e4-85a7-77c30281de26.JPG)

An example simple path that starts from the top left of the grid and terminates at the bottom right.

![hamiltonian path](https://cloud.githubusercontent.com/assets/6858645/6261395/0196e6e8-b7b4-11e4-9654-e3664430d13a.JPG)

An example Hamiltonian path.

## Tile Paths

![hamiltonian tile path](https://cloud.githubusercontent.com/assets/6858645/6261379/e8deefc4-b7b3-11e4-805b-4bb22e623343.JPG)

1 out of the 214,480 Hamiltonian tile paths.

![simple tile path](https://cloud.githubusercontent.com/assets/6858645/6261380/ea63d184-b7b3-11e4-8d2d-f548b06ca4b9.JPG)

1 out of the 1,595,655 simple tile paths.
