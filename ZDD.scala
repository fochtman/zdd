import Graph._
import scala.collection.mutable.{ HashMap, ListBuffer }
import collection.immutable.ListMap

object ZDD {
  abstract class ZDD
  // ELM == edge-label, and mates
  case class ELM(edgeLabel: Edge, mates: Map[Vertex, Vertex]) extends ZDD {
    override def toString = "--edge("+ edgeLabel.u +","+ edgeLabel.v +") mates["+ mates +"]--"
  }
  // node
  case class Node(params: ELM, loChild: ZDD, hiChild: ZDD) extends ZDD {
    override def toString = "NODE_[params("+ params +") lo("+ loChild +") hi("+ hiChild +")]__"
  }

  case object zeroTerminal extends ZDD {
    val value = 0
  }

  case object oneTerminal extends ZDD {
    val value = 1
  }

  def prettyPrintZDD(zddList: List[Node]): Unit = {
    var inc = 0
    var zddNodeStrs = (zddList map(_.toString)).mkString("\n")
    for (p <- zddList) {
      zddNodeStrs = zddNodeStrs.replaceAllLiterally(p.params.toString, inc.toString)
      inc += 1
    }
    println(zddNodeStrs)
  }


  def setupFrontier(g: Graph, domain: Map[Int, List[Vertex]]) = {
    val firstEdge = g.edges(0)
    val rootMates: Map[Vertex, Vertex] = ListMap(domain(0) zip g.vertices:_*)
    val rootParams = ELM(firstEdge, rootMates)
    val frontier = HashMap[Int, Set[ELM]](0 -> Set(rootParams))
    Range(1, g.edges.length) map (i => frontier(i) = Set())
    frontier
  }

  def dom(i: Int, edges: List[Edge]): List[(Int, List[Vertex])] = edges match {
    case Nil => Nil

    case head :: tail =>
      val sortedDom = edges.flatMap(e =>
        List(e.u, e.v)).distinct.sortWith((v0,v1) => v0 < v1)
      (i, sortedDom) :: Nil ::: dom(i+1, tail)
  }

  def getZeroChild(i: Int, g: Graph, n: ELM, domain: Map[Int, List[Vertex]]): ELM = {
    val removal = (domain(i).toSet &~ domain(i+1).toSet).toList
    val mates =
      if (removal.isEmpty)
        n.mates
      else
        n.mates - removal(0)
    ELM(g.edges(i+1), mates)
  }

  def rejectEdge(n: ELM, edge: Edge): Boolean = {
    if (n.mates(edge.u) == 0 || n.mates(edge.u) == edge.v)
      true
    else if (n.mates(edge.v) == 0 || n.mates(edge.v) == edge.u)
      true
    else
      false
  }

  def getOneChild(i: Int, g: Graph, n: ELM, domain: Map[Int, List[Vertex]]): ELM = {
    val edge = g.edges(i)
    val edgeSet = Set(edge.u, edge.v)
    val zero: Vertex = 0

    val mateUpdate =
      for {
        (w, _) <- n.mates
        if domain(i+1).contains(w)
      } yield {
        if (edgeSet.contains(w) && n.mates(w) != w)
          zero
          //Vertex(0)
        else if (n.mates(w) == edge.u)
          n.mates(edge.v)
        else if (n.mates(w) == edge.v)
          n.mates(edge.u)
        else
          n.mates(w)
      }
    val mates = ListMap(domain(i+1) zip mateUpdate:_*)
    ELM(g.edges(i+1), mates)
  }

  def addNextFrontier(i: Int, N: HashMap[Int, Set[ELM]], children: List[ZDD]): Unit = children match {
    case Nil => ()

    case (head: ELM) :: tail =>
      N(i) = N(i) + head
      addNextFrontier(i, N, tail)

    case `zeroTerminal` :: tail =>
      addNextFrontier(i, N, tail)

    case `oneTerminal` :: tail =>
      addNextFrontier(i, N, tail)

    case _ => ()
  }

  def countZDDOnePaths(root: ZDD): Int = {
    val resultTable = HashMap[ZDD, Int]()

    def countHelper(node: ZDD): Int = {
      if (node == `zeroTerminal`) 0
      else if (node == `oneTerminal`) 1
      else if (resultTable.contains(node)) 1
      else {
        node match {
          case Node(p, lo, hi) =>
            resultTable(node) = countHelper(lo) + countHelper(hi)
            resultTable(node)
        }
      }
    }
    countHelper(root)
  }

  def enumZDDValidPaths(root: Node): ListBuffer[ListBuffer[Byte]] = {
    val z: Byte = 0
    val o: Byte = 1
    val pathBuffer = ListBuffer[ListBuffer[Byte]]()

    def helper(n: Node, path: ListBuffer[Byte]): Unit = n match {

      case Node(_, lo: Node, hi: Node) =>
        helper(lo, path :+ z)
        helper(hi, path :+ o)

      case Node(_, `zeroTerminal`, `zeroTerminal`) =>

      case Node(_, `zeroTerminal`, `oneTerminal`) =>
        (path += o) +=: pathBuffer

      case Node(_, `oneTerminal`, `zeroTerminal`) =>
        (path += z) +=: pathBuffer

      case Node(_, `oneTerminal`, `oneTerminal`) =>
        (path += z) +=: pathBuffer
        (path += o) +=: pathBuffer

      case Node(_, `zeroTerminal`, hi: Node) =>
        helper(hi, path :+ o)

      case Node(_, `oneTerminal`, hi: Node) =>
        (path :+ z) +=: pathBuffer
        helper(hi, path :+ o)

      case Node(_, lo: Node, `zeroTerminal`) =>
        helper(lo, path :+ z)

      case Node(_, lo: Node, `oneTerminal`) =>
        (path :+ o) +=: pathBuffer
        helper(lo, path :+ z)
    }

    helper(root, new ListBuffer[Byte]())

    pathBuffer
  }

  def buildZDD(zddList: List[Node]): Node = {
    val mapZDD = Map(zddList map(node =>
      (node.params, node)):_*)

    val rootZDD = mapZDD(zddList(0).params)

    def helperFunc(node: Node): Node = node match {
      case Node(p: ELM, lo: ELM, hi: ELM) =>
        Node(p, helperFunc(mapZDD(lo)), helperFunc(mapZDD(hi)))

      case Node(p: ELM, terminal, hi: ELM) =>
        Node(p, terminal, helperFunc(mapZDD(hi)))

      case Node(p: ELM, lo: ELM, terminal) =>
        Node(p, helperFunc(mapZDD(lo)), terminal)

      case n @ Node(p: ELM, _, _) => n
    }
    helperFunc(rootZDD)
  }

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

    /*
    if for some v that exists in n.edgeLabel diff domain(i+1) one of the following holds, the 0-child is incompatible:
      if (v exists in hSet && mate(v) == v)
        then (m,i,0) is incompatible
      else if (v !exists in hSet && mate(v) !exist in {0, v})
        then (m,i,0) is incompatible
   */
  def zeroChildIsIncompatible(i: Int, n: ELM, h: List[VertexPair], hSet: Set[Vertex], domain: Map[Int, List[Vertex]]): Boolean = {
    val edge = n.edgeLabel
    val mateTable = n.mates

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

    def matchV2(vertexList: List[Vertex]): Boolean = vertexList match {
      case Nil => false

      case (u: Vertex) :: tail =>
        if (mateTable(u) == u)
          true
        else
          matchV2(tail)
    }

    val nextDom =
      if (i + 1 < domain.size) domain(i + 1)
      else Nil

    val res0 = matchV(List(edge.u, edge.v) diff nextDom)
    // uncomment for increased restrictions
    //val res1 = matchV2(domain(i) diff nextDom)
    //res0 || res1
    res0
  }

  def oneChildIsIncompatible(i: Int, g: Graph, n: ELM, h: List[VertexPair], hSet: Set[Vertex], domain: Map[Int, List[Vertex]]): Boolean = {
      val edge = n.edgeLabel
      val mateTable = n.mates
      val nextDom =
        if (i +1 < domain.size) domain(i + 1)
        else Nil
      val hUnionVDiff = hSet union (g.vertices.toSet diff nextDom.toSet)
      val mateU = mateTable(edge.u)
      val mateV = mateTable(edge.v)

      if (hSet.contains(edge.v) && mateTable(edge.v) != edge.v)
        true
      else if (hSet.contains(edge.u) && mateTable(edge.u) != edge.u)
        true
      else if (hUnionVDiff.contains(mateU) && hUnionVDiff.contains(mateV) && !h.contains(VertexPair(mateU, mateV)))
        true
      else
        false
  }


  def algorithmTwo(g: Graph, h: List[VertexPair]): Node = {
    val domain = Map.empty ++ dom(0, g.edges)
    val frontier = setupFrontier(g, domain)
    val edgeIndices = g.edges.indices.toList
    val hSet = h.flatMap(p => List(p.v0, p.v1)).toSet

    def quickSolution(i: Int, n: ELM): Boolean = {
      for (u <- domain(i+1); v <- g.vertices)
        if (u != v && n.mates(u) == v && h.contains(VertexPair(u, v))) return true
      false
    }

    val zddList: List[Node] =
      for {
        i <- edgeIndices
        n <- frontier(i)
      } yield {
        /*
        For both 0-child and 1-child when i == |E| + 1, they are assumed to lead to
        one terminal
         */

        val zeroChild =
          if (zeroChildIsIncompatible(i, n, h, hSet, domain))
            zeroTerminal
          else if (i+1 < edgeIndices.length)
            getZeroChild(i, g, n, domain)
          else
            oneTerminal

        val oneChildPossibility =
          if (rejectEdge(n, g.edges(i)))
            zeroTerminal
          else if (oneChildIsIncompatible(i, g, n, h, hSet, domain))
            zeroTerminal
          else if (i+1 < edgeIndices.length)
            getOneChild(i, g, n, domain)
          else
            oneTerminal

        val oneChild =
          if (i+1 < domain.size && quickSolution(i, n))
            oneTerminal
          else
            oneChildPossibility
        addNextFrontier(i+1, frontier, List(zeroChild, oneChild))

        //could just return a 3 tuple, turn it into a node later
        Node(n, zeroChild, oneChild)
      }
    buildZDD(zddList)
  }
}
