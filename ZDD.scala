import Graph._
import scala.annotation.tailrec
import scala.collection.mutable.{ ListBuffer }
import collection.immutable.ListMap
import System.{currentTimeMillis => _time}

object ZDD {

  trait ZDD {
    def edgeLabel: Edge
    def mates: Map[Vertex, Vertex]
    def loChild: ZDD
    def hiChild: ZDD
  }

  object zeroTerminal extends ZDD {
    def edgeLabel = throw new NoSuchElementException("zeroTerminal.edgeLabel")
    def mates = throw new NoSuchElementException("zeroTerminal.mates")
    def loChild = throw new NoSuchElementException("zeroTerminal.loChild")
    def hiChild = throw new NoSuchElementException("zeroTerminal.hiChild")
    override def toString = "0Term"
  }

  object oneTerminal extends ZDD {
    def edgeLabel = throw new NoSuchElementException("oneTerminal.edgeLabel")
    def mates = throw new NoSuchElementException("oneTerminal.mates")
    def loChild = throw new NoSuchElementException("oneTerminal.loChild")
    def hiChild = throw new NoSuchElementException("oneTerminal.hiChild")
    override def toString = "1Term"
  }

  class Node(
    val edgeLabel: Edge,
    val mates: Map[Vertex, Vertex],
    var loChild: ZDD,
    var hiChild: ZDD
) extends ZDD {

    def isEmpty: Boolean = {
      if (loChild == null) true
      else if (hiChild == null) true
      else false
    }

    def get: Node = this

    val _1: Edge = edgeLabel
    val _2: Map[Vertex, Vertex] = mates
    def _3: ZDD = loChild
    def _4: ZDD = hiChild

    override def equals(other: Any): Boolean = {
      other match {
        case that: Node =>
          (that canEqual this) &&
            edgeLabel == that.edgeLabel &&
            mates == that.mates

        case _ => false
      }
    }

    def canEqual(other: Any) = other.isInstanceOf[Node]

    override def hashCode: Int = {
      41 * (
        41 + edgeLabel.hashCode
      ) + mates.hashCode
    }

    override def toString = "NODE_["+ edgeLabel +"]("+ mates +") lo("+ loChild +") hi("+ hiChild +")]__"
  }

  object Node {
    def unapply(n: Node) = n
    def apply(e: Edge, m: Map[Vertex, Vertex]) = new Node(e, m, null, null)
  }

  def setupDomain(i: Int, edges: List[Edge]): Map[Int, List[Vertex]] = {
    // Helper function dom returns a List((i,List(vertex0,v1,...,vK),...,(i+?,List(vK-1,vK))
    def dom(i: Int, edges: List[Edge]): List[(Int, List[Vertex])] =
      edges match {
        case Nil => Nil

        case head :: tail =>
          val sortedDom = edges.flatMap(e =>
            List(e.u, e.v)).distinct.sortWith((v0, v1) => v0 < v1)
          (i, sortedDom) :: Nil ::: dom(i + 1, tail)
      }
    Map.empty ++ dom(0, edges)
  }

  def setupFrontier(g: Graph, domain: Map[Int, List[Vertex]]) = {
    val rootMates: Map[Vertex, Vertex] = ListMap(domain(0) zip g.vertices:_*)
    val root = Node(g.edges(0), rootMates)
    val frontier = scala.collection.mutable.HashMap[Int, Set[Node]](0 -> Set(root))
    Range(1, g.edges.length) map (i =>
      frontier(i) = Set())
    frontier
  }

  def enumZDDValidPaths(root: Node): ListBuffer[ListBuffer[Byte]] = {
    val z: Byte = 0
    val o: Byte = 1
    val pathBuffer = ListBuffer[ListBuffer[Byte]]()

    def helper(n: Node, path: ListBuffer[Byte]): Unit = n match {

      case Node(_, _, lo: Node, hi: Node) =>
        helper(lo, path :+ z)
        helper(hi, path :+ o)

      case Node(_, _, `zeroTerminal`, hi: Node) =>
        helper(hi, path :+ o)

      case Node(_, _, `oneTerminal`, hi: Node) =>
        (path :+ z) +=: pathBuffer
        helper(hi, path :+ o)

      case Node(_, _, lo: Node, `zeroTerminal`) =>
        helper(lo, path :+ z)

      case Node(_, _, lo: Node, `oneTerminal`) =>
        (path :+ o) +=: pathBuffer
        helper(lo, path :+ z)

      case Node(_, _, `zeroTerminal`, `zeroTerminal`) =>

      case Node(_, _, `zeroTerminal`, `oneTerminal`) =>
        (path += o) +=: pathBuffer

      case Node(_, _, `oneTerminal`, `zeroTerminal`) =>
        (path += z) +=: pathBuffer

      case Node(_, _, `oneTerminal`, `oneTerminal`) =>
        (path += z) +=: pathBuffer
        (path += o) +=: pathBuffer
    }

    helper(root, new ListBuffer[Byte]())

    pathBuffer
  }

  /*
  For both 0-child and 1-child when i == |E| + 1, they are assumed to lead to
  one terminal
   */
  def algorithmTwo(g: Graph, h: List[VertexPair]): Node = {
    val startT = _time
    var t = _time

    val edges = g.edges
    val domain = setupDomain(0, edges)
    val frontier = setupFrontier(g, domain)

    // collections which are used frequently
    val edgeIndices = edges.indices.toList
    val vertexSet = g.vertices.toSet
    val hSet = h.flatMap(p => List(p.v0, p.v1)).toSet

    val iMax = edgeIndices.length
    assert(iMax == domain.size, "domain.size != edgeIndices.length")

    val hamiltonianPath = false

    val setupT = _time - t

    var zeroChildT = ListBuffer[Long]()
    var oneChildTP = ListBuffer[Long]()
    var oneChildT = ListBuffer[Long]()
    var frontT = ListBuffer[Long]()

    /*
    Start zeroChild calculations
     */
    def zeroChildIsIncompatible(i: Int, n: Node): Boolean = {
      val edge = n.edgeLabel
      val mateTable = n.mates
      val nextDomain =
        if (i + 1 < iMax) domain(i + 1)
        else Nil

      @tailrec
      def matchV(vertexList: List[Vertex]): Boolean = vertexList match {
        case Nil => false

        case (v: Vertex) :: Nil =>
          if (hSet.contains(v) && mateTable(v) == v)
            true
          else if (!hSet.contains(v) && (mateTable(v) != 0 && mateTable(v) != v))
            true
          else
            false

        case (u: Vertex) :: v =>
          if (hSet.contains(u) && mateTable(u) == u)
            true
          else if (!hSet.contains(u) && (mateTable(u) != 0 && mateTable(u) != u))
            true
          else
            matchV(v)
      }

      @tailrec
      def matchV2(vertexList: List[Vertex]): Boolean = vertexList match {
        case Nil => false

        case (u: Vertex) :: tail =>
          if (mateTable(u) == u) {
            true
          }
          else
            matchV2(tail)
      }

      if (hamiltonianPath) {
        val res0 = matchV(List(edge.u, edge.v) diff nextDomain)
        val res1 = matchV2(domain(i) diff nextDomain)
        res0 || res1
      } else {
        matchV(List(edge.u, edge.v) diff nextDomain)
      }
    }

    def getZeroChild(i: Int, n: Node): Node = {
      // could pre-calculate these domain differences
      val removal = domain(i) diff domain(i+1)
      val mates =
        if (removal.isEmpty)
          n.mates
        else
          n.mates - removal(0)
      Node(edges(i+1), mates)
    }

    def decideZeroChild(i: Int, n: Node): ZDD = {
      if (zeroChildIsIncompatible (i, n) )
        zeroTerminal
      else if (i + 1 < iMax)
        getZeroChild (i, n)
      else
        oneTerminal
    }
    /*
    End zeroChild calculations
     */

    /*
    Start oneChild calculations
     */

    def rejectEdge(n: Node, edge: Edge): Boolean = {
      val u = edge.u
      val v = edge.v
      val nMates = n.mates

      if (nMates(u) == 0 || nMates(u) == v)
        true
      else if (nMates(v) == 0 || nMates(v) == u)
        true
      else
        false
    }

    /*
    if for some v that exists in n.edgeLabel diff domain(i+1) one of the following holds, the 0-child is incompatible:
      if (v exists in hSet && mate(v) == v)
        then (m,i,0) is incompatible
      else if (v !exists in hSet && mate(v) !exist in {0, v})
        then (m,i,0) is incompatible
   */
    def oneChildIsIncompatible(i: Int, n: Node): Boolean = {
      if (rejectEdge(n, edges(i)))
        true
      else {
        val nextDom =
          if (i + 1 < iMax) domain(i + 1)
          else Nil
        val hUnionVDiff = hSet union (vertexSet diff nextDom.toSet)
        val u = n.edgeLabel.u
        val v = n.edgeLabel.v
        val nMates = n.mates
        val mateOfU = nMates(u)
        val mateOfV = nMates(v)

        if (hSet.contains(v) && nMates(v) != v)
          true
        else if (hSet.contains(u) && nMates(u) != u)
          true
        else if (hUnionVDiff.contains(mateOfU) && hUnionVDiff.contains(mateOfV) && !h.contains(VertexPair(mateOfU, mateOfV)))
          true
        else
          false
      }
    }

    def getOneChild(i: Int, n: Node, nextDomain: List[Vertex]): Node = {
      val edge = edges(i)
      val u = edge.u
      val v = edge.v
      val edgeSet = Set(u, v)
      val nMates = n.mates
      //val zero: Vertex = 0

      val mateUpdate =
        for {
          (w, _) <- nMates
          if nextDomain.contains(w)
        } yield {
          if (edgeSet.contains(w) && nMates(w) != w)
            0
          else if (nMates(w) == u)
            nMates(v)
          else if (nMates(w) == v)
            nMates(u)
          else
            nMates(w)
        }

      val mateTuples = nextDomain zip mateUpdate
      val mates = ListMap(mateTuples:_*)
      Node(edges(i+1), mates)
    }

    def decideOneChild(i: Int, n: Node): ZDD = {
      if (oneChildIsIncompatible(i, n))
        zeroTerminal
      else if (i+1 < iMax)
        getOneChild(i, n, domain(i+1))
      else
        oneTerminal
    }
    /*
    End oneChild calculations
     */

    def quickSolution(i: Int, n: Node): Boolean = {
      /*
      println(n)
      val v = n.edgeLabel.v
      //for (u <- domain(i+1)) { //}; v <- g.vertices) {
      for (u <- domain(i)) { //}; v <- g.vertices) {
        println(u, v, n.mates(u))
        if (u != v && n.mates(u) == v && h.contains(VertexPair(u, v))) return true
        else if (u != v && n.mates(u) != u && h.contains(VertexPair(n.mates(u), v))) return true
      }
      */
      false

    }
    println("h: "+ h)

    var counter = 0
    for (i <- edgeIndices; n <- frontier(i)) {
      counter += 1
      t = _time //zeroChild time
      val zeroChild = decideZeroChild(i, n)
      zeroChildT += (_time - t)

      t = _time //oneChildP time
      val oneChild = decideOneChild(i, n)
      oneChildTP += (_time - t)

      /*
      println("Frontier size: "+frontier(i).size)
      println("nextFrontier: "+ frontier(i))
      println("zero: "+zeroChild)
      println("one: "+oneChild)
      println()
      */
      n.loChild = zeroChild
      n.hiChild = oneChild

      t = _time //front time
      zeroChild match {
        case n: Node => frontier(i+1) += n
        case _ =>
      }
      oneChild match {
        case n: Node => frontier(i+1) += n
        case _ =>
      }
      frontT += (_time - t)
    }

    println("setupT: "    +setupT)
    println("zeroChildT: "+zeroChildT.sum)
    println("oneChildTP: "+oneChildTP.sum)
    println("oneChildT: " +oneChildT.sum)
    println("frontT: "    +frontT.sum)
    println("sumT: "      +(zeroChildT.sum + oneChildTP.sum + oneChildT.sum + frontT.sum + setupT))
    println("total: "+(_time - startT))
    println("counter: "+counter)

    assert(frontier(0).size == 1, "more than one root node, eh?")
    // return the root of the zdd
    frontier(0).head
  }
}

/*
def algorithmOne(g: Graph) = {
  val domain = Map.empty ++ dom(0, g.edges)
  val frontier = setupFrontier(g, domain)
  val edgeIndices = g.edges.indices.toList

  val zddList: List[Node] =
    for {
      i <- edgeIndices
      n <- frontier(i)
    } yield {

      val zeroChild =
        if (i+1 < edgeIndices.length)
          getZeroChild(i, g, n, domain)
        else
          oneTerminal

      val oneChild =
        if (rejectEdge(n, g.edges(i)))
          zeroTerminal
        else if (i+1 < edgeIndices.length)
          getOneChild(i, g, n, domain)
        else
          oneTerminal

      addNextFrontier(i+1, frontier, List(zeroChild, oneChild))
      Node(n, zeroChild, oneChild)
    }
  buildZDD(zddList)
}
*/
