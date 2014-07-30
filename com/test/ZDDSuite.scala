package com.test

import com.main.{ZDDMain, Graph}
import ZDDMain._
import Graph._

import org.scalatest.{GivenWhenThen, FeatureSpec}

/*
class SetSpec extends FlatSpec with GivenWhenThen {

  "A mutable Set" should "allow an element to be added" in {
    Given("an empty mutable Set")
    val set = mutable.Set.empty[String]

    When("an element is added")
    set += "clarity"

    Then("the Set should have size 1")
    assert(set.size === 1)

    And("the Set should contain the added element")
    assert(set.contains("clarity"))

    info("That's all folks!")
  }
}
*/

class ZDDSuite extends FeatureSpec with GivenWhenThen {
  trait NodeEquality {
    val n0 = Node(Edge(1, 2), Map[Vertex, Vertex](1 -> 1, 2 -> 2))
    val n1 = Node(Edge(1, 2), Map[Vertex, Vertex](1 -> 1, 2 -> 2))
  }

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
}
