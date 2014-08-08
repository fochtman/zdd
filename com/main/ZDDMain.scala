package com.main

import UnderlyingGraph._
import scala.annotation.tailrec
import scala.collection.mutable.{ListBuffer, Set => FrontierSet}

object ZDDMain {

  trait ZDD {
    def edgeLabel: Edge
    def mates: Map[Vertex, Vertex]
    def zeroChild: ZDD
    def oneChild: ZDD
  }

  abstract class Terminal

  object zeroTerminal extends Terminal with ZDD {
    def edgeLabel = throw new NoSuchElementException("zeroTerminal.edgeLabel")
    def mates = throw new NoSuchElementException("zeroTerminal.mates")
    def zeroChild = throw new NoSuchElementException("zeroTerminal.zeroChild")
    def oneChild = throw new NoSuchElementException("zeroTerminal.oneChild")
    override def toString = "0Term"
  }

  object oneTerminal extends Terminal with ZDD {
    def edgeLabel = throw new NoSuchElementException("oneTerminal.edgeLabel")
    def mates = throw new NoSuchElementException("oneTerminal.mates")
    def zeroChild = throw new NoSuchElementException("oneTerminal.zeroChild")
    def oneChild = throw new NoSuchElementException("oneTerminal.oneChild")
    override def toString = "1Term"
  }

  class Node(
    val edgeLabel: Edge,
    val mates: Map[Vertex, Vertex],
    var zeroChild: ZDD,
    var oneChild: ZDD
  ) extends ZDD {

    def isEmpty: Boolean = {
      if (zeroChild == null) true
      else if (oneChild == null) true
      else false
    }

    def get: Node = this

    val _1: Edge = edgeLabel
    val _2: Map[Vertex, Vertex] = mates
    def _3: ZDD = zeroChild
    def _4: ZDD = oneChild

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

    override def toString = "NODE_[" + edgeLabel + "](" + mates + ") lo(" + zeroChild + ") hi(" + oneChild + ")]__"
  }

  object Node {
    def unapply(n: Node) = n
    def apply(e: Edge, m: Map[Vertex, Vertex]) = new Node(e, m, null, null)
  }

  def setupDomain(edges: List[Edge]): Map[Int, List[Vertex]] = {
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

  def domainDifferences(xs: List[List[Vertex]]): List[List[Vertex]] = {
    xs match {
      case Nil =>
        Nil
      case head :: Nil =>
        head :: domainDifferences(Nil)
      case head :: tail =>
        (head diff tail.head) :: domainDifferences(tail)
    }
  }

  def setupFrontier(g: Graph, domain: Map[Int, List[Vertex]]) = {
    //val rootMates: Map[Vertex, Vertex] = ListMap(domain(0) zip g.vertices: _*)
    val rootMates: Map[Vertex, Vertex] = Map(domain(0) zip g.vertices: _*)
    val root = Node(g.edges(0), rootMates)
    val frontier = scala.collection.mutable.HashMap[Int, FrontierSet[Node]](0 -> FrontierSet(root))
    Range(1, g.edges.length) map (i =>
      frontier(i) = FrontierSet[Node]())
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

      case Node(_, _, null, null) =>

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

  def numberLink(g: Graph, h: List[VertexPair], hamiltonianPaths: Boolean): Node = {
    val edges = g.edges.toVector
    val vertices = g.vertices

    val domain = setupDomain(g.edges)
    val frontier = setupFrontier(g, domain)

    // collections which are used frequently
    val edgeIndices = edges.indices.toList
    val vertexSet = vertices.toSet
    val hSet = h.flatMap(p => List(p.v0, p.v1)).toSet
    val domainDiff = domainDifferences(domain.values.toList.sortBy(_.length).reverse).toVector

    val iMax = edgeIndices.length
    assert(iMax == domain.size, "domain.size != edgeIndices.length")

    val updateMates = true
    val dontUpdateMates = false

    def getNode(i: Int, updateMatesChoice: Boolean, nMates: Map[Vertex, Vertex]): Node = {
      val currentEdge = edges(i)
      val nextEdge = edges(i + 1)
      val u = currentEdge.u
      val v = currentEdge.v
      val frontierSet = frontier(i + 1)

      val nextMates =
        if (!updateMatesChoice) {
          nMates
        } else {
          val edgeSet = Set(u, v)
          for {
            (w, mate) <- nMates
          } yield {
            if (edgeSet.contains(w) && nMates(w) != w)
              (w, 0)
            else if (nMates(w) == u)
              (w, nMates(v))
            else if (nMates(w) == v)
              (w, nMates(u))
            else
              (w, mate) //nMates(w)
          }
        }

      val newNode =
        if (domainDiff(i).isEmpty)
          Node(nextEdge, nextMates)
        else
          Node(nextEdge, nextMates - domainDiff(i).head)

      if (frontierSet.contains(newNode)) {
        frontierSet.find(_ == newNode).get
      } else {
        frontierSet += newNode
        newNode
      }
    }

    def rejectEdge(nMates: Map[Vertex, Vertex], edge: Edge): Boolean = {
      val u = edge.u
      val v = edge.v

      if (Set(0, v).contains(nMates(u)))
        true
      else if (Set(0, u).contains(nMates(v)))
        true
      else
        false
    }


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

      if (hamiltonianPaths) {
        val res0 = matchV(List(edge.u, edge.v) diff nextDomain)
        val res1 = matchV2(domain(i) diff nextDomain)
        res0 || res1
      } else {
        matchV(List(edge.u, edge.v) diff nextDomain)
      }
    }

    def decideZeroChild(i: Int, n: Node): ZDD = {
      if (zeroChildIsIncompatible(i, n))
        zeroTerminal
      else if (i + 1 < iMax) {
        getNode(i, dontUpdateMates, n.mates)
      }
      else
        oneTerminal
    }
    /*
    End zeroChild calculations
     */

    /*
    Start oneChild calculations
     */

    /*
    if for some v that exists in n.edgeLabel diff domain(i+1) one of the following holds, the 0-child is incompatible:
      if (v exists in hSet && mate(v) == v)
        then (m,i,0) is incompatible
      else if (v !exists in hSet && mate(v) !exist in {0, v})
        then (m,i,0) is incompatible
   */
    def oneChildIsIncompatible(i: Int, n: Node): Boolean = {
      if (rejectEdge(n.mates, edges(i)))
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

    def decideOneChild(i: Int, n: Node): ZDD = {
      if (oneChildIsIncompatible(i, n))
        zeroTerminal
      else if (i + 1 < iMax)
        getNode(i, updateMates, n.mates)
      else
        oneTerminal
    }
    /*
    End oneChild calculations
     */

    /*
    Main loop of numLink function
     */
    for (i <- edgeIndices; n <- frontier(i)) {
      n.zeroChild = decideZeroChild(i, n)
      n.oneChild = decideOneChild(i, n)
    }
    frontier(0).head
  }
}

  /*
  def algorithmOne(g: Graph): Node = {
    val edges = g.edges

    val domain = setupDomain(edges)
    val frontier = setupFrontier(g, domain)

    val edgeIndices = g.edges.indices.toList
    val iMax = edgeIndices.length

    println(frontier)

    for (i <- edgeIndices; n <- frontier(i)) {

      n.zeroChild =
        if (i + 1 < iMax) {
          getNode (
            edges(i + 1),
            restrictMates(n.mates, domain(i) diff domain(i + 1)),
            frontier(i + 1)
          )
        } else {
          oneTerminal
        }

      n.oneChild =
        if (rejectEdge(n.mates, edges(i)))
          zeroTerminal
        else if(i + 1 < iMax)
          getNode (
            edges(i + 1),
            restrictMates(
              mateUpdate(edges(i), n.mates), domain(i) diff domain(i + 1)
            ),
            frontier(i + 1)
          )
        else
          oneTerminal
    }

    frontier(0).head
  }
  */

  /*
  def restrictMates(nMates: Map[Vertex, Vertex], domainDifference: List[Vertex]): Map[Vertex, Vertex] = {
    if (domainDifference.isEmpty)
      nMates
    else
      nMates - domainDifference.head
  }

  def getNode(edge: Edge, nMates: Map[Vertex, Vertex], frontierSet: FrontierSet[Node]): Node = {
    val newNode = Node(edge, nMates)

    frontierSet.find(_ == newNode) match {
      case Some(extantNode) => extantNode
      case None =>
        frontierSet += newNode
        newNode
    }
  }

  def rejectEdge(nMates: Map[Vertex, Vertex], edge: Edge): Boolean = {
    val u = edge.u
    val v = edge.v

    if (Set(0, v).contains(nMates(u)))
      true
    else if (Set(0, u).contains(nMates(v)))
      true
    else
      false
  }

  def mateUpdate(edge: Edge, nMates: Map[Vertex, Vertex]): Map[Vertex, Vertex] = {
    val u = edge.u
    val v = edge.v
    val edgeSet = Set(u, v)

    for {
      (w, mate) <- nMates
    } yield {
      if (edgeSet.contains(w) && nMates(w) != w)
        (w, 0)
      else if (nMates(w) == u)
        (w, nMates(v))
      else if (nMates(w) == v)
        (w, nMates(u))
      else
        (w, mate) //nMates(w)
    }
  }
  */

