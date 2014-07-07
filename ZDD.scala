import Graph._
import scala.collection.mutable.HashMap
import collection.immutable.ListMap

object ZDD {
  abstract class ZDD
  // ELM == edge-label, and mates
  case class ELM[T](edgeLabel: Edge[T], mates: Map[Vertex[T], Vertex[T]]) extends ZDD {
    override def toString = "--edge("+ edgeLabel.u +","+ edgeLabel.v +") mates["+ mates +"]--"
  }
  // node
  case class Node[T](params: ELM[T], loChild: ZDD, hiChild: ZDD) extends ZDD {
    override def toString = "NODE_[params("+ params +") lo("+ loChild +") hi("+ hiChild +")]__"
  }

  case object zeroTerminal extends ZDD {
    val value = 0
  }

  case object oneTerminal extends ZDD {
    val value = 1
  }

  def prettyPrintZDD[T](zddList: List[Node[T]]): Unit = {
    var inc = 0
    var zddNodeStrs = (zddList map(_.toString)).mkString("\n")
    for (p <- zddList) {
      zddNodeStrs = zddNodeStrs.replaceAllLiterally(p.params.toString, inc.toString)
      inc += 1
    }
    println(zddNodeStrs)
  }


  def setupFrontier[T](g: Graph[T], domain: Map[Int, List[Vertex[T]]]) = {
    val firstEdge = g.edges(0)
    val rootMates = getMates(domain(0), g.vertices)
    val rootParams = ELM(firstEdge, rootMates)
    val frontier = HashMap[Int, Set[ELM[T]]](0 -> Set(rootParams))
    Range(1, g.edges.length) map (i => frontier(i) = Set())
    frontier
  }

  def dom[T](i: Int, edges: List[Edge[T]]): List[(Int, List[Vertex[T]])] = edges match {
    case Nil => Nil
    case head :: tail =>
      val sortedDom = edges.flatMap(e => List(e.u, e.v)).distinct.sortWith((v0,v1) => v0.order < v1.order)
      (i, sortedDom) :: Nil ::: dom(i+1, tail)
  }

  def getMates[T](edgeDomain: List[Vertex[T]], m: List[Vertex[T]]): Map[Vertex[T], Vertex[T]] = {
    ListMap(edgeDomain zip m:_*)
  }

  def getZeroChild[T](i: Int, g: Graph[T], n: ELM[T], domain: Map[Int, List[Vertex[T]]]): ELM[T] = {
    val removal = (domain(i).toSet &~ domain(i+1).toSet).toList
    val mates = if (!removal.isEmpty) n.mates - removal(0) else n.mates
    ELM(g.edges(i+1), mates)
  }

  def rejectEdge[T](n: ELM[T], thisEdge: Edge[T]): Boolean = {
    if ((n.mates(thisEdge.u).order == -1) || (n.mates(thisEdge.u) == thisEdge.v)) true
    else if ((n.mates(thisEdge.v).order == -1) || (n.mates(thisEdge.v) == thisEdge.u)) true
    else false
  }

  def getOneChild[T](i: Int, g: Graph[T], n: ELM[T], domain: Map[Int, List[Vertex[T]]]): ELM[T] = {
    val thisEdge = g.edges(i)
    val edgeSet = Set(thisEdge.u, thisEdge.v)
    val mateUpdate =
      for ((w, notUsed) <- n.mates; if domain(i+1).contains(w)) yield {
        if (edgeSet.contains(w) && n.mates(w) != w) w match {
          case Vertex(id, order: Int) => Vertex(id, -1)
        }
        else if (n.mates(w) == thisEdge.u) n.mates(thisEdge.v)
        else if (n.mates(w) == thisEdge.v) n.mates(thisEdge.u)
        else n.mates(w)
      }
    val mates = ListMap(domain(i+1) zip mateUpdate:_*)
    ELM(g.edges(i+1), mates)
  }

  def addNextFrontier[T](i: Int, N: HashMap[Int, Set[ELM[T]]], children: List[ZDD]): Unit = children match {
    case Nil => ()
    case (head: ELM[T]) :: tail =>
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
          case Node(_, lo, hi) =>
            resultTable(node) = countHelper(lo) + countHelper(hi)
            resultTable(node)
        }
      }
    }
    countHelper(root)
  }

  def buildZDD[T](zddList: List[Node[T]]): Node[T] = {
    val mapZDD = Map(zddList map(node =>
      (node.params, node)):_*)

    val rootZDD = mapZDD(zddList(0).params)

    def helperFunc(node: Node[T]): Node[T] = node match {
      case Node(p: ELM[T], lc: ELM[T], hc: ELM[T]) =>
        Node(p, helperFunc(mapZDD(lc)), helperFunc(mapZDD(hc)))

      case Node(p: ELM[T], lc, hc: ELM[T]) =>
        Node(p, lc, helperFunc(mapZDD(hc)))

      case Node(p: ELM[T], lc: ELM[T], hc) =>
        Node(p, helperFunc(mapZDD(lc)), hc)

      case Node(p: ELM[T], lc, hc) =>
        Node(p, lc, hc)
    }
    helperFunc(rootZDD)
  }

  def algorithmOne[T](g: Graph[T]): Node[T] = {
    val domain = Map.empty ++ dom(0, g.edges)
    val frontier = setupFrontier(g, domain)
    val edgeIndices = g.edges.indices.toList

    val zddList: List[Node[T]] =
      for {
        i <- edgeIndices
        n <- frontier(i)
      } yield {

        val zeroChild = if (i+1 < edgeIndices.length) getZeroChild(i, g, n, domain)
          else oneTerminal

        val oneChild = if (rejectEdge(n, g.edges(i))) zeroTerminal
          else if (i+1 < edgeIndices.length) getOneChild(i, g, n, domain)
          else oneTerminal

        addNextFrontier(i+1, frontier, List(zeroChild, oneChild))
        Node(n, zeroChild, oneChild)
      }
    buildZDD(zddList)
  }

  def zeroChildIsIncompatible[T](i: Int, n: ELM[T], h: List[VertexPair[T]], hSet: Set[Vertex[T]], domain: Map[Int, List[Vertex[T]]]): Boolean = {
    val edge = n.edgeLabel
    val mateTable = n.mates

    def matchV(vertexList: List[Vertex[T]]): Boolean = vertexList match {
      case Nil => false
      case (v: Vertex[T]) :: rest =>
        if (hSet.contains(v) && mateTable(v) == v) true
        else if (!hSet.contains(v) && mateTable(v).order != -1 && mateTable(v) != v) true
        else false
    }

    if (i + 1 < domain.size)
      matchV(List(edge.u, edge.v) diff domain(i+1))
    else
      matchV(List(edge.v))
  }

  def oneChildIsIncompatible[T](i: Int, g: Graph[T], n: ELM[T], h: List[VertexPair[T]], hSet: Set[Vertex[T]], domain: Map[Int, List[Vertex[T]]]): Boolean = {
    if (i + 1 < g.edges.length) {
      val edge = n.edgeLabel
      val mateTable = n.mates
      // can move this hFlat out of loop, only needs to be calculated once
      //val hFlat = h.flatMap(p => List(p.v0, p.v1)).toSet
      val hUnionVDiff = hSet union (g.vertices.toSet diff domain(i + 1).toSet)
      val mateU = mateTable(edge.u)
      val mateV = mateTable(edge.v)

      if (hSet.contains(edge.v) && mateTable(edge.v) != edge.v) true
      else if (hSet.contains(edge.u) && mateTable(edge.u) != edge.u) true
      else if (hUnionVDiff.contains(mateU) && hUnionVDiff.contains(mateV) && !h.contains(VertexPair(mateU, mateV))) true
      else false
    } else {
      false
    }
  }

  def algorithmTwo[T](g: Graph[T], h: List[VertexPair[T]]): Node[T] = {
    val domain = Map.empty ++ dom(0, g.edges)
    val frontier = setupFrontier(g, domain)
    val edgeIndices = g.edges.indices.toList
    val hSet = h.flatMap(p => List(p.v0, p.v1)).toSet

    val zddList: List[Node[T]] =
      for {
        i <- edgeIndices
        n <- frontier(i)
      } yield {

        val zeroChild =
          if (zeroChildIsIncompatible(i, n, h, hSet, domain)) zeroTerminal
          else if (i+1 < edgeIndices.length) getZeroChild(i, g, n, domain)
          else oneTerminal

        val oneChildTmp =
          if (rejectEdge(n, g.edges(i))) zeroTerminal
          else if (oneChildIsIncompatible(i, g, n, h, hSet, domain)) zeroTerminal
          else if (i+1 < edgeIndices.length) getOneChild(i, g, n, domain)
          else oneTerminal

        val oneChild = oneChildTmp match {
          case `zeroTerminal` => oneChildTmp
          case `oneTerminal` => oneChildTmp
          case ELM(e, mate) =>
            val quickSolution =
            for  {
              (u,v) <- mate
              if u != v
              if mate(u) == v || mate(v) == u
              if h.contains(VertexPair(u, v)) || h.contains(VertexPair(v, u))
            } yield (u,v)

            if (quickSolution.isEmpty) oneChildTmp
            else oneTerminal
        }
        addNextFrontier(i+1, frontier, List(zeroChild, oneChild))
        Node(n, zeroChild, oneChild)
      }
    prettyPrintZDD(zddList)
    buildZDD(zddList)
  }
}
