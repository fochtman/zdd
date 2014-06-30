/**
 * Created by Tyler on 6/22/2014.
 */
object Graph {
 case class Vertex[T](id: T, order: Int) {
   override def toString = ""+id+""
 }
 case class Edge[T](u: Vertex[T], v: Vertex[T])
 case class Graph[T](vertices: List[Vertex[T]], edges: List[Edge[T]])
}
