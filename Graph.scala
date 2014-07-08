object Graph {

  //case class Vertex(id: Int) { //}, order: Int) {
  case class Vertex(order: Int) { //}, order: Int) {
    //override def toString = "["+id+","+order+"]"
    //override def toString = "v("+id+")"
    override def toString = ""+order+""
  }

  case class Edge(u: Vertex, v: Vertex)

  case class Graph(vertices: List[Vertex], edges: List[Edge])

  case class VertexPair(v0: Vertex, v1: Vertex)

  /* lexicographical comparison
   * edge order: {i, j} < {i', j'} iff (i < i') or (i == i' and j < j')
   */
  def compareEdgeOrder(a: Edge, b: Edge) = {
    if (a.u.order < b.u.order) true
    else if (a.u.order == b.u.order && a.v.order < b.v.order) true
    else if (a.u.order == b.u.order && a.v.order == b.v.order) false
    else false
  }

  class GridGraph(m: Int, n: Int) {
    val rowNum = m
    val colNum = n
    val graph = buildGridGraph(m, n)
    val vertexToCoord =
      Map(graph.vertices zip
        ((1 to m).toList flatMap(i =>
          (1 to n).toList map(j =>
            (j, i)))):_*) /* Note: (j, i) */

    def buildGridGraph(m: Int, n: Int): Graph = {
      val rows = (1 to m).toList
      val cols = (1 to n).toList

      val vertexList =
        (1 to rowNum * colNum).toList map(i =>
          Vertex(i))

      val nodeCoords =
        rows flatMap(i =>
          cols map(j =>
            (i, j)))

      val coordToVertex = Map(nodeCoords zip vertexList: _*)

      val horizontalEdges =
        rows.flatMap(i =>
          cols.withFilter(j => j < n).map(j => ((i, j), (i, j + 1))))

      val verticalEdges =
        rows.withFilter(i => i < m).flatMap(i =>
          cols.map(j => ((i, j), (i + 1, j))))

      val allEdges =
        (horizontalEdges ::: verticalEdges) map(e =>
          Edge(coordToVertex(e._1), coordToVertex(e._2)))

      // a heuristic to (generally) reduce the size of the resulting zdd
      val edgeList = allEdges.sortWith(compareEdgeOrder)
      Graph(vertexList, edgeList)
    }
  }

}
