object UnderlyingGraph {

  type Vertex = Int

  case class Edge(u: Vertex, v: Vertex)

  case class Graph(vertices: List[Vertex], edges: List[Edge])

  case class VertexPair(v0: Vertex, v1: Vertex)

  /* lexicographical comparison
   * edge order: {i, j} < {i', j'} iff (i < i') or (i == i' and j < j')
   */
  def compareEdgeOrder(a: Edge, b: Edge) = {
    if (a.u < b.u) true
    else if (a.u == b.u && a.v < b.v) true
    else if (a.u == b.u && a.v == b.v) false
    else false
  }

  case class GridGraph(m: Int, n: Int) {
    val rowNum = m
    val colNum = n
    val (graph, horizontalEdges, verticalEdges) = buildGridGraph(m, n)
    val vertexToCoord =
      Map(graph.vertices zip
        ((1 to m).toList flatMap(i =>
          (1 to n).toList map(j =>
            (j-1, i-1)))):_*) /* Note: (j, i) */

    def buildGridGraph(m: Int, n: Int): (Graph, Vector[Edge], Vector[Edge]) = {
      val rows = (1 to m).toList
      val cols = (1 to n).toList

      val vertices = (1 to rowNum * colNum).toList

      val nodeCoords =
        rows flatMap(i =>
          cols map(j =>
            (i, j)))

      val coordToVertex = Map(nodeCoords zip vertices: _*)

      val horizontalEdges =
        rows flatMap(i =>
          cols withFilter(j =>
            j < n) map(j =>
              ((i, j), (i, j + 1))))

      val verticalEdges =
        rows withFilter(i =>
          i < m) flatMap(i =>
            cols map(j =>
              ((i, j), (i + 1, j))))

      val hEdges =
        horizontalEdges map(e =>
          Edge(coordToVertex(e._1), coordToVertex(e._2)))

      val vEdges =
        verticalEdges map(e =>
          Edge(coordToVertex(e._1), coordToVertex(e._2)))

      // a heuristic to (generally) reduce the size of the resulting zdd
      val edges = (hEdges ::: vEdges).sortWith(compareEdgeOrder)

      (Graph(vertices, edges), hEdges.toVector, vEdges.toVector)
    }
  }
}
