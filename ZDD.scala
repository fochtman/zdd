/**
 * Created by Tyler on 6/24/2014.
 */
import Graph._

import scala.collection.mutable.HashMap
import collection.immutable.ListMap
//import scala.reflect.ClassTag

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

  def compareVertOrder[T](v0: Vertex[T], v1: Vertex[T]) = v0.order < v1.order

  def dom[T](i: Int, edges: List[Edge[T]]): List[(Int, List[Vertex[T]])] = edges match {
    case Nil => Nil
    case head :: tail =>
      val sortedDom = edges.flatMap(e => List(e.u, e.v)).distinct.sortWith(compareVertOrder)
      (i, sortedDom) :: Nil ::: dom(i+1, tail)
  }

  def getMates[T](edgeDomain: List[Vertex[T]], m: List[Vertex[T]]): Map[Vertex[T], Vertex[T]] = {
    val tupleList = for ((v0,v1) <- edgeDomain zip m) yield {(v0, v1)}
    ListMap(tupleList:_*)
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
    val mates = ListMap((for ((v,v0)<- domain(i+1) zip mateUpdate) yield {(v,v0)}):_*)
    ELM(g.edges(i+1), mates)
  }

  def buildFrontier[T](i: Int, N: HashMap[Int, Set[ELM[T]]], children: List[ZDD]): Unit = children match {
    case Nil => ()
    case (head: ELM[T]) :: tail =>
      N(i) = N(i) + head
      buildFrontier(i, N, tail)
    case `zeroTerminal` :: tail =>
      buildFrontier(i, N, tail)
    case `oneTerminal` :: tail =>
      buildFrontier(i, N, tail)
    case _ => ()
  }

  def countZDDOnePaths(root: ZDD): Int = {
    val resultTable = HashMap[ZDD, Int]()
    def patMat(node: ZDD): Int = node match {
      case Node(_, lo, hi) =>
        resultTable(node) = countHelper(lo) + countHelper(hi)
        resultTable(node)
    }
    def countHelper(node: ZDD): Int = {
      if (node == `zeroTerminal`) 0
      else if (node == `oneTerminal`) 1
      else if (resultTable.contains(node)) 0
      else patMat(node)
    }
    countHelper(root)
  }
  /* UTILITY FUNCTIONS
  def printHead(xs: List[ZDD]): Unit = xs match {
    case Nil => println("finished")
    case Node(p, lc, rc) :: tail =>
      println(p+"  "+lc+"  "+"  "+rc)
      printHead(tail)
  }
  def checkTerm[T](node: Node[T]): Unit = node match {
    case Node(_, l: Node[T], _) =>
      func(l)
    case Node(_, `oneTerminal`, _) => println("oneT")
  }
  func(rootz)
  */

  def buildZDD[T](zddList: List[Node[T]]): Node[T] = {
    val paramsNode = for {
      n <- zddList.toList
      p = n.params
    } yield (p,n)

    val mapZDD = Map.empty ++ paramsNode
    val zddElem = mapZDD(paramsNode(0)._1)

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
    helperFunc(zddElem)
  }

  def algorithmOne[T](g: Graph[T]): Node[T] = {
    val domain = Map.empty ++ dom(0, g.edges)
    val firstEdge = g.edges(0)
    val rootMates = getMates(domain(0), g.vertices)
    val rootParams = ELM(firstEdge, rootMates)
    val N = HashMap[Int, Set[ELM[T]]]()
    N(0) = Set(rootParams)
    val numEdges = g.edges.length
    Range(1, numEdges) map (i => N(i) = Set())

    val zddList: List[Node[T]] =
      for (i <- g.edges.indices.toList; n <- N(i)) yield {
        val zeroChild =
          if (i+1 < numEdges) getZeroChild(i, g, n, domain)
          else                oneTerminal
        val oneChild =
          if (rejectEdge(n, g.edges(i)))  zeroTerminal
          else if (i+1 < numEdges)        getOneChild(i, g, n, domain)
          else                            oneTerminal

        buildFrontier(i+1, N, List(zeroChild, oneChild))
        Node(n, zeroChild, oneChild)
      }

    buildZDD(zddList.toList)
  }

  def zeroChildIsIncompatible[T](i: Int, n: ELM[T], h: List[VertexPair[T]], domain: Map[Int, List[Vertex[T]]]): Boolean = {
    val edge = n.edgeLabel
    val mateTable = n.mates
    val hFlat = h.flatMap(p => List(p.v0, p.v1))

    def matchV(vertexList: List[Vertex[T]]): Boolean = vertexList match {
      case Nil => false
      case (v: Vertex[T]) :: rest =>
        if (hFlat.contains(v) && mateTable(v) == v) true
        else if (!hFlat.contains(v) && mateTable(v).order != -1 && mateTable(v) != v) true
        else false
    }

    if (i + 1 < domain.size) matchV(List(edge.u, edge.v) diff domain(i+1))
    else {
      matchV(List(edge.v))
      /*
      val result1 = matchV(List(edge.v))
      val result2 = matchV(List(edge.u))
      println("r1: "+ result1)
      println("r2: "+ result2)
      result1
      */
    }

  }

  def oneChildIsIncompatible[T](i: Int, g: Graph[T], n: ELM[T], h: List[VertexPair[T]], domain: Map[Int, List[Vertex[T]]]): Boolean = {
    if (i + 1 < g.edges.length) {
      val edge = n.edgeLabel
      val mateTable = n.mates
      // can move this hFlat out of loop, only needs to be calculated once
      val hFlat = h.flatMap(p => List(p.v0, p.v1)).toSet
      val hUnionVDiff = hFlat union (g.vertices.toSet diff domain(i + 1).toSet)
      val mateU = mateTable(edge.u)
      val mateV = mateTable(edge.v)

      if (hFlat.contains(edge.v) && mateTable(edge.v) != edge.v) {println(2); true}
      else if (hFlat.contains(edge.u) && mateTable(edge.u) != edge.u) {println(3); true}
      else if (hUnionVDiff.contains(mateU) && hUnionVDiff.contains(mateV) && !h.contains(VertexPair(mateU, mateV)) ) {println(4); true}
      else false
    } else {
      false
    }
  }

  def algorithmTwo[T](g: Graph[T], h: List[VertexPair[T]]): Node[T] = {
    // make function for boilerplate for algo 1, 2
    val domain = Map.empty ++ dom(0, g.edges)
    val firstEdge = g.edges(0)
    val rootMates = getMates(domain(0), g.vertices)
    val rootParams = ELM(firstEdge, rootMates)
    val N = HashMap[Int, Set[ELM[T]]]()
    N(0) = Set(rootParams)
    val numEdges = g.edges.length
    Range(1, numEdges) map (i => N(i) = Set())

    val zddList: List[Node[T]] =
      for (i <- g.edges.indices.toList; n <- N(i)) yield {
        // basically need to keep what we have, but add more
        val zeroChild =
          if (zeroChildIsIncompatible(i, n, h, domain)) zeroTerminal
          else if (i+1 < numEdges) getZeroChild(i, g, n, domain)
          else oneTerminal

        val oneChild =
          if (rejectEdge(n, g.edges(i))) zeroTerminal
          else if (oneChildIsIncompatible(i, g, n, h, domain))  zeroTerminal
          //else if (haveSolutionSkipSubPaths(n, h))
          else if (i+1 < numEdges)  getOneChild(i, g, n, domain)
          else oneTerminal

        buildFrontier(i+1, N, List(zeroChild, oneChild))
        Node(n, zeroChild, oneChild)
      }
    zddList.foreach(println)
    buildZDD(zddList.toList)
  }
}
