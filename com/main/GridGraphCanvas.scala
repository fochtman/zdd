package com.main

import java.awt.RenderingHints
import java.lang.System.{currentTimeMillis => _time}
import com.main.UnderlyingGraph._
import com.main.ZDDMain._
import com.main.T1TilePaths._
import com.main.BDD.{algoTwo, enumZDDValidPaths2}
import scala.collection.mutable.ListBuffer
import scala.swing._

class GridGraphCanvas(dim: java.awt.Dimension) extends Panel {

  var pathEdges = ListBuffer[ListBuffer[Byte]]()
  var currentPath = List[Byte]()

  // the height and width of the grid squares
  private val jump = dim.getHeight.toInt / 10

  def computeAxis(totalAxisUnits: Int): Vector[Int] = {
    val axisOrigin = (size.width - (jump * (totalAxisUnits - 1))) / 2
    val axisMax = axisOrigin + (totalAxisUnits - 1) * jump
    (Range(axisOrigin, axisMax) by jump).toVector
  }

  def computePathAxis(axis: Vector[Int]): Vector[Int] = {
    axis :+ (axis.last + jump)
  }

  def computePathCoordinates(path: List[Byte]): List[((Int, Int), (Int, Int))] = {
    val pathMap = path.zipWithIndex filter (x =>
      x._1 == 1) map (index =>
      vis.grid.graph.edges(index._2))

    pathMap map (edge =>
      (vis.grid.vertexToCoord(edge.u), vis.grid.vertexToCoord(edge.v)))
  }

  override def paintComponent(g: Graphics2D) {
    g.setBackground(zorn.titaniumWhite)
    g.clearRect(0, 0, size.width, size.height)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    g.setStroke(stroke.eight)
    g.setColor(zorn.ivoryBlack)

    val xAxis = computeAxis(vis.grid.colNum)
    val yAxis = computeAxis(vis.grid.rowNum)

    xAxis map (x =>
      yAxis map (y =>
        g.drawRect(x, y, jump, jump)))

    g.setStroke(stroke.roundSixteen)
    g.setColor(zorn.yellowOchreAlpha)

    val xPathAxis = computePathAxis(xAxis)
    val yPathAxis = computePathAxis(yAxis)
    val pathCoords = computePathCoordinates(currentPath)

    pathCoords map { case (u, v) =>
      g.drawLine(xPathAxis(u._1), yPathAxis(u._2), xPathAxis(v._1), yPathAxis(v._2))
    }
  }

  def changePath(sliderValue: Int): Unit = {
    currentPath = pathEdges(sliderValue).toList
    repaint()
  }

  def collectPathEdges(choice: Int): Unit = {
    val ggV: List[UnderlyingGraph.Vertex] = vis.grid.graph.vertices
    val h = List(VertexPair(ggV(0), ggV.last))

    choice match {
      case 1 =>
        val hamiltonianPaths = true
        val zddRoot: ZDDMain.Node = numberLink(vis.grid.graph, h, hamiltonianPaths)
        pathEdges = enumZDDValidPaths(zddRoot)

      case 2 =>
        val hamiltonianPaths = false
        val zddRoot: ZDDMain.Node = numberLink(vis.grid.graph, h, hamiltonianPaths)
        pathEdges = enumZDDValidPaths(zddRoot)
    }
  }
}

