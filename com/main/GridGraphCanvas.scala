package com.main

import java.awt.RenderingHints
import java.lang.System.{currentTimeMillis => _time}

import com.main.UnderlyingGraph._
import com.main.ZDDMain._
import com.main.T1TilePaths._
import com.main.BDD.{algoTwo, enumZDDValidPaths2}

import scala.collection.mutable.ListBuffer
import scala.swing._

/**
 * Created by Tyler on 8/2/2014.
 */
class GridGraphCanvas(dim: java.awt.Dimension) extends Panel {

  var pathEdges = ListBuffer[ListBuffer[Byte]]()
  var currentPathCoords = List[((Int, Int), (Int, Int))]()


  // the height and width of the grid squares
  private val jump = dim.getHeight.toInt / 10

  private var xOrigin = (size.width - (jump * (vis.grid.colNum - 1))) / 2
  private var yOrigin = (size.height - (jump * (vis.grid.rowNum - 1))) / 2

  private var xMax = xOrigin + (vis.grid.colNum-1) * jump
  private var xAxis = (Range(xOrigin, xMax) by jump).toList

  private var yMax = yOrigin + (vis.grid.rowNum-1) * jump
  private var yAxis = (Range(yOrigin , yMax) by jump).toList
  /*
  The xAxis and yAxis contain points which correspond the to top
  left corner of each square in the grid. So when drawing the paths
  over the grid, we need one extra point at the end of each axis list.
   */
  private var xPathAxis = xAxis ::: (xAxis.last + jump :: Nil)
  private var yPathAxis = yAxis ::: (yAxis.last + jump :: Nil)

  def updateGraphCanvasFields(): Unit = {
    xOrigin = (size.width - (jump * (vis.grid.colNum - 1))) / 2
    yOrigin = (size.height - (jump * (vis.grid.rowNum - 1))) / 2

    xMax = xOrigin + (vis.grid.colNum - 1) * jump
    xAxis = (Range(xOrigin, xMax) by jump).toList

    yMax = yOrigin + (vis.grid.rowNum - 1) * jump
    yAxis = (Range(yOrigin, yMax) by jump).toList

    xPathAxis = xAxis ::: (xAxis.last + jump) :: Nil
    yPathAxis = yAxis ::: (yAxis.last + jump) :: Nil
  }


  override def paintComponent(g: Graphics2D) {
    updateGraphCanvasFields()

    g.setBackground(zorn.titaniumWhite)
    g.clearRect(0, 0, size.width, size.height)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    g.setStroke(stroke.eight)
    g.setColor(zorn.ivoryBlack)

    xAxis map (x =>
      yAxis map (y =>
        g.drawRect(x, y, jump, jump)))

    g.setStroke(stroke.roundSixteen)
    g.setColor(zorn.yellowOchreAlpha)

    currentPathCoords map { case (u, v) =>
      g.drawLine(xPathAxis(u._1), yPathAxis(u._2),
        xPathAxis(v._1), yPathAxis(v._2))
    }
  }

  def changePath(sliderValue: Int): Unit = {
    val byteStr = pathEdges(sliderValue)

    val pathMap = byteStr.zipWithIndex filter (x =>
      x._1 == 1) map (index =>
      vis.grid.graph.edges(index._2))

    currentPathCoords = pathMap.toList map (edge =>
      (vis.grid.vertexToCoord(edge.u), vis.grid.vertexToCoord(edge.v)))

    repaint()
  }

  def time[R](block: => R, funcName: String): R = {
    val t0 = _time
    val result = block
    val t1 = _time
    println(funcName +"\telapsed time: " + (t1 - t0) + "ms")
    result
  }

  def collectPathEdges(choice: Int): Unit = choice match {
    case 1 =>
      val ggV: List[UnderlyingGraph.Vertex] = vis.grid.graph.vertices
      val h = List(VertexPair(ggV(0), ggV.last))
      val zddRoot: BDD.Node = time (algoTwo(vis.grid.graph, h), "BDDAlgo2 =>")
      pathEdges = time (enumZDDValidPaths2(zddRoot), "Path finding =>\t")
      println("Algo2 Number of valid paths: "+ pathEdges.length +"\n")
    /*
    println("Here in algo1...")
    val zddRoot: ZDDMain.Node = time (algorithmOne(vis.grid.graph), "Algo1 =>")
    pathEdges = time (enumZDDValidPaths(zddRoot), "Path finding =>\t")
    //pathEdges.foreach(println)
    println("Algo1 Number of valid paths: "+ pathEdges.length +"\n")
    */

    case 2 =>
      val ggV: List[UnderlyingGraph.Vertex] = vis.grid.graph.vertices
      val h = List(VertexPair(ggV(0), ggV.last))
      val zddRoot: ZDDMain.Node = time (numberLink(vis.grid.graph, h), "Algo2 =>")
      pathEdges = time (enumZDDValidPaths(zddRoot), "Path finding =>\t")
      println("Algo2 Number of valid paths: "+ pathEdges.length +"\n")

  }
}

