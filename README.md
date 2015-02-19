# ZDDs + tile-based algorithmic self-assembly

This program enumerates the set of paths between two points in a lattice. 

Users can choose whether they want for this path set to consist of all paths or only Hamiltonian paths.

Additionally users can determine whether a given temperature 1 tile set can be assembled such that
every vertex along the path is covered. This assembly must be well-formed, that is, the tile edges
along the path must match. 

At the heart of the calculations I'm utilizing a data structure known as a zero-supressed decision diagram (ZDD).

![set parameters](Examples/zdd-parameters.jpg)

*enumerate paths*

![simple path](Examples/zdd-example-vis-0.jpg)

![hamiltonian path](Examples/zdd-example-vis-1.jpg)

*enumerate tile paths*

![simple tile path](Examples/zdd-example-vis-with-tiles-0.jpg)

1 out of the 214,480 simple tile paths.

![hamiltonian tile path](Examples/zdd-example-vis-with-tiles-1.jpg)

1 out of the 1,595,655 hamiltonian tile paths.
