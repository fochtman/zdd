object Graph {

  case class Vertex[T](id: T, order: Int) {
    //override def toString = "v("+id+", "+order+")"
    override def toString = ""+id+""
  }

  case class Edge[T](u: Vertex[T], v: Vertex[T])

  case class Graph[T](vertices: List[Vertex[T]], edges: List[Edge[T]])

  case class VertexPair[T](v0: Vertex[T], v1: Vertex[T])

  /* lexicographical comparison
   * edge order: {i, j} < {i', j'} iff (i < i') or (i == i' and j < j')
   */
  def compareEdgeOrder[T](a: Edge[T], b: Edge[T]) = {
    if (a.u.order < b.u.order) true
    else if (a.u.order == b.u.order && a.v.order < b.v.order) true
    else if (a.u.order == b.u.order && a.v.order == b.v.order) false
    else false
  }

  class GridGraph(m: Int, n: Int) {
    val rowNum = m
    val colNum = n
    val graph = buildGridGraph(rowNum, colNum)

    def buildGridGraph(m: Int, n: Int): Graph[Int] = {
      val rows = (1 to m).toList
      val cols = (1 to n).toList
      val vertexList = (1 to m * n).toList.map(i => Vertex(i, i - 1))
      val nodeCoords = rows.flatMap(i => cols.map(j => (i, j)))
      val ijToVertex = Map(nodeCoords zip vertexList: _*)

      val horizontalEdges =
        rows.flatMap(i =>
          cols.withFilter(j => j < n).map(j => ((i, j), (i, j + 1))))
      val verticalEdges =
        rows.withFilter(i => i < m).flatMap(i =>
          cols.map(j => ((i, j), (i + 1, j))))

      val hEdges = horizontalEdges.map(e => Edge(ijToVertex(e._1), ijToVertex(e._2)))
      val vEdges = verticalEdges.map(e => Edge(ijToVertex(e._1), ijToVertex(e._2)))

      // a heuristic to (generally) reduce the size of the resulting zdd
      val edgeList = (hEdges ::: vEdges).sortWith(compareEdgeOrder)

      Graph(vertexList, edgeList)
    }
  }

}
