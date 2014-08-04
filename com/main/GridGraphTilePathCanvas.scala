package com.main

import java.awt.RenderingHints
import java.lang.System.{currentTimeMillis => _time}

import com.main.UnderlyingGraph._
import com.main.ZDDMain._
import com.main.T1TilePaths._
import com.main.BDD.{algoTwo, enumZDDValidPaths2}

import scala.collection.mutable.ListBuffer
import scala.swing._

class GridGraphTilePathCanvas(dim: java.awt.Dimension) extends Panel {

  var pathEdges = ListBuffer[ListBuffer[Byte]]()
  var byteStr = ListBuffer[Byte]()

  var pathsToTilePaths = Map[ListBuffer[Byte], ListBuffer[ListBuffer[Tile]]]()
  var tilePaths = ListBuffer[ListBuffer[Tile]]()
  var pathSet = Set[Set[Vertex]]()

  var currentPathCoords = List[((Int, Int), (Int, Int))]()
  var currentTileCoords = List[(Int, Int)]()


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

    println("xAxis "+xPathAxis)
    println("yAxis "+yPathAxis)
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
      g.drawLine(xPathAxis(u._1), yPathAxis(u._2), xPathAxis(v._1), yPathAxis(v._2))
    }
    g.setStroke(stroke.four)
    g.setColor(zorn.cadmiumRedMedium)
    g.setFont(new Font("Ariel", java.awt.Font.ITALIC, 14))
    drawTiles(g)
  }


  def drawTiles(g: Graphics2D): Unit = {
    def helper(currTileCoords: List[(Int, Int)]): Unit = {
      currTileCoords match {
        case Nil =>
        case head :: tail=>
          g.drawRect(xPathAxis(head._1)-32, yPathAxis(head._2)-32, jump, jump)
          helper(tail)
      }
    }
    helper(currentTileCoords)
  }
  //var counter = 1
  /*
  g.drawString(s"$counter", xPathAxis(head._1)-28, yPathAxis(head._2)+28)
  val strr = s"${xPathAxis(head._1)},${yPathAxis(head._2)}"
  g.drawString(strr, xPathAxis(head._1)-28, yPathAxis(head._2))
  counter += 1
  */

  def orderedVertexVector(pathSet: Set[Set[Int]], start: Vertex): Vector[Vertex] = {
    def helper(ps: Set[Set[Int]], u: Vertex): List[Vertex] = {
      val uv = ps.filter(s => s.contains(u))
      assert(uv.size == 1)
      val v = (uv.head - u).head

      if (ps.size == 1)
        v :: Nil
      else
        v :: helper(ps - uv.head, v)
    }
    Vector(start) ++ helper(pathSet, start).toVector
  }

  def updateTilePathsFields(): Unit = {
    tilePaths = pathsToTilePaths(byteStr)
    pathSet = buildPathSet(byteStr, vis.grid.graph.edges)
    val vv = orderedVertexVector(pathSet, vis.h.head.v0)
    currentTileCoords = vv.toList map (vertex =>
      vis.grid.vertexToCoord(vertex))
  }

  def changePath(pathSliderValue: Int): Unit = {
    byteStr = pathEdges(pathSliderValue)

    updateTilePathsFields()
    gridWithTilesVis.setTilePathSlider()

    val pathMap = byteStr.zipWithIndex filter (x =>
      x._1 == 1) map (index =>
      vis.grid.graph.edges(index._2))

    currentPathCoords = pathMap.toList map (edge =>
      (vis.grid.vertexToCoord(edge.u), vis.grid.vertexToCoord(edge.v)))

    repaint()
  }

  def changeTilePath(tilePathSliderValue: Int): Unit = {
    val tiles = tilePaths(tilePathSliderValue)
    println(tiles)
  }

  def time[R](block: => R, funcName: String): R = {
    val t0 = _time
    val result = block
    val t1 = _time
    println(funcName +"\telapsed time: " + (t1 - t0) + "ms")
    result
  }

  def collectPathEdges(choice: Int): Unit = {
    val ggV: List[UnderlyingGraph.Vertex] = vis.grid.graph.vertices
    val h = List(VertexPair(ggV(0), ggV.last))

    choice match {
      case 1 =>
        val hamiltonianPaths = true
        val zddRoot: ZDDMain.Node = time(numberLink(vis.grid.graph, h, hamiltonianPaths), "Algo2 =>")
        pathEdges = time(enumZDDValidPaths(zddRoot), "Path finding =>\t")
        println("Algo2 Number of valid paths: " + pathEdges.length + "\n")

      case 2 =>
        val hamiltonianPaths = false
        val zddRoot: ZDDMain.Node = time(numberLink(vis.grid.graph, h, hamiltonianPaths), "Algo2 =>")
        pathEdges = time(enumZDDValidPaths(zddRoot), "Path finding =>\t")
        println("Algo2 Number of valid paths: " + pathEdges.length + "\n")
    }

    collectTilePaths()

  }

  def collectTilePaths(): Unit = {
    println("tmpTileLink")
    val sq = T1TilePaths.Glue('a'.toInt)
    val wt = T1TilePaths.Glue('b'.toInt)
    val dt = T1TilePaths.Glue('c'.toInt)
    val ci = T1TilePaths.Glue('d'.toInt)
    val cc = T1TilePaths.Glue('e'.toInt)

    val a = Tile(nullGlue, nullGlue, sq, wt)
    val b = Tile(sq, ci, nullGlue, nullGlue)
    val c = Tile(dt, nullGlue, nullGlue, ci)
    val d = Tile(nullGlue, wt, dt, nullGlue)
    val e = Tile(cc, cc, cc, cc)
    val f = Tile(nullGlue, cc, nullGlue, cc)
    //val e = Tile(nullGlue, nullGlue, nullGlue, nullGlue)
    val alpha = TileSet(Set(a, b, c, d, e, f))
    //val alpha = TileSet(Set(a, b, c, d))

    pathsToTilePaths = mapPathsToTilePaths(vis.h, pathEdges, vis.grid, alpha)

    var totals = 0
    for ((k, v) <- pathsToTilePaths)
      totals += v.length

    println("Totals: "+ totals)
  }
}
