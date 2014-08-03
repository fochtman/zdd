package com.main

import scala.collection.mutable.HashMap
import com.main.ZDDMain._
import java.awt.RenderingHints
import scala.swing._

/**
 * Created by Tyler on 8/2/2014.
 */
class DAGCanvas extends Panel {

  trait DAG {
    def x: Int
    def y: Int
    def width: Int
    def height: Int
  }

  case class DAGEdge(x0: Int, y0: Int, x1: Int, y1: Int)

  case class DAGNode(x: Int, y: Int, width: Int, height: Int, lo: DAG, hi: DAG) extends DAG

  case class DAGLeaf(x: Int, y: Int, width: Int, height: Int) extends DAG


  val nodeWH = 8
  val jump = nodeWH * 2
  val fullJump = jump * 2

  var startX = fullJump
  var startY = jump

  var DAGEdges = List[DAGEdge]()
  var DAGNodes = List[DAGLeaf]()

  //val dummyLeaf = DAGLeaf(fullJump, fullJump, nodeWH, nodeWH)
  //var currentDAG = List[DAG](dummyLeaf)

  def drawDAGEdges(g: Graphics2D, node: DAG): Unit = {

    val half = nodeWH / 2
    val nodeCenter = (n: DAG) => (n.x + half, n.y + half)

    def helper(n: DAG): Unit = {
      n match {

        case DAGNode(x, y, w, h, lo: DAG, hi: DAG) =>
          val parent = nodeCenter(n)

          g.setStroke(stroke.fourDashed)
          val leftChild = nodeCenter(lo)
          g.drawLine(parent._1, parent._2, leftChild._1, leftChild._2)

          g.setStroke(stroke.four)
          val rightChild = nodeCenter(hi)
          g.drawLine(parent._1, parent._2, rightChild._1, rightChild._2)

          helper(lo)
          helper(hi)

        case _ =>
      }
    }

    helper(node)
  }

  def drawDAGNodes(g: Graphics2D, node: DAG): Unit = {
    node match {
      case DAGNode(x, y, w, h, lo: DAG, hi: DAG) =>
        g.fillOval(x, y, w, h)
        drawDAGNodes(g, lo)
        drawDAGNodes(g, hi)
      case DAGLeaf(x, y, w, h) =>
        g.fillOval(x, y, w, h)
    }
  }

  override def paintComponent(g: Graphics2D) {
    g.setBackground(zorn.titaniumWhite)

    g.clearRect(0, 0, size.width, size.height)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    g.setColor(zorn.ivoryBlack)

    g.setStroke(stroke.four)
    //drawDAGEdges(g, currentDAG.head)

    g.setStroke(stroke.four)
    DAGNodes map (n =>
      g.fillOval(n.x, n.y, n.width, n.height))
    //drawDAGNodes(g, currentDAG.head)

  }

  def getNodesPerLevel(depth: Int): scala.collection.mutable.HashMap[Int, Int] = {
    val npl = scala.collection.mutable.HashMap[Int, Int]()
    Range(1, depth) map (i =>
      npl(i) = 0)

    def helper(node: ZDD, i: Int): Unit = {
      node match {
        case Node(_,_, lo: Node, hi: Node) =>
          npl(i) += 1
          helper(lo, i+1)
          helper(hi, i+1)

        case Node(_, _, lo: Terminal, hi: Node) =>
          npl(i) += 1
          helper(hi, i+1)

        case Node(_, _, lo: Node, hi: Terminal) =>
          npl(i) += 1
          helper(lo, i+1)

        case Node(_, _, lo: Terminal, hi: Terminal) =>
          npl(i) += 1

        case _ =>
      }
    }

    helper(vis.root, 1)
    npl(depth) = 2
    npl
  }

  def collectDAGEdges(root: DAG, zero: DAG, one: DAG, npl: HashMap[Int, Int]): List[DAGLeaf] = {

    val half = nodeWH / 2
    val nodeCenter = (n: DAG) => (n.x + half, n.y + half)

    def buildNodeCore(i: Int): DAGLeaf = {
      val x = npl(i)
      npl(i) -= 1
      DAGLeaf(x*fullJump, i*fullJump, nodeWH, nodeWH)
    }

    def helper(n: ZDD, i: Int): List[DAGLeaf] = {
      n match {
        case Node(_, _, lo: Node, hi: Node) =>
          val hiNode = buildNodeCore(i)
          val loNode = buildNodeCore(i)

          /*
          Note!! The issue with algorithmTwo seems to be with getting the lo child
           */
          loNode :: hiNode :: helper(hi, i+1) //::: helper(lo, i+1)

        case Node(_, _, `zeroTerminal`, hi: Node) =>
          val hiNode = buildNodeCore(i)
          hiNode :: helper(hi, i+1)

        case Node(_, _, `oneTerminal`, hi: Node) =>
          val hiNode = buildNodeCore(i)
          hiNode :: helper(hi, i+1)

        case Node(_, _, lo: Node, `zeroTerminal`) =>
          val loNode = buildNodeCore(i)
          loNode :: helper(lo, i+1)

        case Node(_, _, lo: Node, `oneTerminal`) =>
          val loNode = buildNodeCore(i)
          loNode :: helper(lo, i+1)

        case Node(_, _, `zeroTerminal`, `zeroTerminal`) =>
          Nil

        case Node(_, _, null, null) =>
          Nil

        case Node(_, _, `zeroTerminal`, `oneTerminal`) =>
          Nil

        case Node(_, _, `oneTerminal`, `zeroTerminal`) =>
          Nil

        case Node(_, _, `oneTerminal`, `oneTerminal`) =>
          Nil

        /*
        case Node(_, _, w, h, lo: DAG, hi: DAG) =>
          val parent = nodeCenter(n)

          val leftChild = nodeCenter(lo)
          g.drawLine(parent._1, parent._2, leftChild._1, leftChild._2)

          val rightChild = nodeCenter(hi)
          g.drawLine(parent._1, parent._2, rightChild._1, rightChild._2)

          helper(lo)
          helper(hi)
        */
        //case _ =>
      }
    }

    helper(vis.root, 2)
  }

  def repaintDAG(): Unit = {
    val depth = vis.grid.graph.edges.length + 1
    val numNodesAtLevel = getNodesPerLevel(depth)
    println(numNodesAtLevel)

    val max = numNodesAtLevel.values.max
    println(max)

    val center = max * fullJump // 2
    val bottom = (depth + 1) * fullJump

    val root = DAGLeaf(center, jump, nodeWH, nodeWH)
    val zero = DAGLeaf(center - fullJump, bottom, nodeWH, nodeWH)
    val one = DAGLeaf(center + fullJump, bottom, nodeWH, nodeWH)

    //DAGNodes = List(root, zero, one)

    //DAGEdges =
    DAGNodes = root :: zero :: one :: Nil ::: collectDAGEdges(root, zero, one, numNodesAtLevel)
    println("DAGNodes: ")
    DAGNodes.foreach(println)
    // gather number of nodes per level
    //currentDAG = List(DAGNode(startX, startY, nodeWH, nodeWH, DAGLeaf(startX - jump, startY + jump, nodeWH, nodeWH), DAGLeaf(startX + jump, startY + jump, nodeWH, nodeWH)))
    repaint()
  }
}

