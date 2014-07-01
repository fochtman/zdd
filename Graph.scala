/**
 * Created by Tyler on 6/22/2014.
 */
object Graph {

 case class Vertex[T](id: T, order: Int) {
   override def toString = ""+id+""
 }

 case class Edge[T](u: Vertex[T], v: Vertex[T])

 case class Graph[T](vertices: List[Vertex[T]], edges: List[Edge[T]])

 case class VertexPair[T](v0: Vertex[T], v1: Vertex[T])
/*
 def gridGraph(m: Int, n: Int): Graph[Int] = {
   val rows = (1 to m).toList
   val cols = (1 to n).toList

   var i = 0

   val vertexList =
    for {
      i <- rows;
      j <- cols;
      i += 1
    } yield Vertex(i, i-1)

 }
  */
}
