package com.test

import com.main.{ZDDMain, UnderlyingGraph}
import ZDDMain._
import UnderlyingGraph._

import org.scalatest.{GivenWhenThen, FeatureSpec}

class ZDDSuite extends FeatureSpec with GivenWhenThen {

  feature("Node equality contract") {
    val n0 = Node(Edge(1, 2), Map[Vertex, Vertex](1 -> 1, 2 -> 2))
    val n1 = Node(Edge(1, 2), Map[Vertex, Vertex](1 -> 1, 2 -> 2))
    val n2 = Node(Edge(1, 2), Map[Vertex, Vertex](1 -> 1, 2 -> 2))

    scenario("reflexive") {
      assert(n0.equals(n0))
    }

    scenario("symmetric") {
      assert(n0.equals(n1) && n1.equals(n0))
    }

    scenario("transitive") {
      assert(n0.equals(n1) && n1.equals(n2) && n0.equals(n2))
    }

    scenario("is non-null") {
      assert(!n0.equals(null))
    }
  }

  /*
  feature("getNode") {
    val g = GridGraph(2, 2).graph
    val domain = setupDomain(g.edges)
    val frontier = setupFrontier(g, domain)
    val root: Node = frontier(0).head
    val i = 0

    scenario("return root, frontier(i) stays same") {
      val frontierBefore = frontier(i)
      val n = getNode(g.edges(i), restrictMates(root.mates, domain(i)), frontier(i))
      assert(n.equals(root))
      assert(frontierBefore.equals(frontier(i)))
    }

    scenario("return new node, and place it in frontier(i+1)") {
      val n0 = getNode(g.edges(i+1), restrictMates(root.mates, domain(i+1)), frontier(i+1))
      val n1 = Node(Edge(1,3), Map(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4))
      assert(n0.equals(n1))
      assert(Set(n1).equals(frontier(i+1)))
    }
  }

  feature("rejectEdge") {
    val edge = Edge(1, 2)
    val mate0 = Map(1 -> 1, 2 -> 2)
    val mate1 = Map(1 -> 2, 2 -> 2)
    val mate2 = Map(1 -> 0, 2 -> 2)
    val mate3 = Map(1 -> 1, 2 -> 1)
    val mate4 = Map(1 -> 1, 2 -> 0)

    scenario("don't reject") {
      assert(!rejectEdge(mate0, edge))
    }

    scenario("reject") {
      assert(rejectEdge(mate1, edge))
      assert(rejectEdge(mate2, edge))
      assert(rejectEdge(mate3, edge))
      assert(rejectEdge(mate4, edge))
    }
  }

  feature("mateUpdate") {
    /*
    The following underlying graph is given as an example on pg. 183 of
    'Finding All Solutions and Instances of Numberlink and Slitherlink by ZDDs'
    by Minato et al.

    These scenarios will be more clear if you take a peak at that graph.
     */

    val (p, q, r, s) = (1, 2, 3, 4)
    val vertices = List(p, q, r, s)
    val edges = List(Edge(p, q), Edge(p, r), Edge(q, r), Edge(q, s))
    val g = Graph(vertices, edges)

    val domain = setupDomain(g.edges)
    val frontier = setupFrontier(g, domain)
    val root: Node = frontier(0).head

    assert(root.equals(Node(Edge(p, q), Map(p -> p, q -> q, r -> r, s -> s))))

    val i = 0
    scenario("update mates by taking Edge(p, q) from root") {
      mateUpdate(Edge(p, q), restrictMates(root.mates, domain(i+1))).equals(Map(p -> q, p -> q, r -> r, s -> s))
    }

    scenario("update a mate table with zero") {
      val n = Node(Edge(q, r), Map(q -> p, s -> s))
      mateUpdate(Edge(q, r), restrictMates(n.mates, domain(i+3))).equals(Map(q -> 0, s -> s))
    }

  }
  */

}
