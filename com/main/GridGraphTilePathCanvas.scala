package com.main

import java.awt.RenderingHints

import com.main.UnderlyingGraph._
import com.main.ZDDMain._
import com.main.T1TilePaths._

import scala.collection.mutable.ListBuffer
import scala.swing._

class GridGraphTilePathCanvas(dim: java.awt.Dimension) extends Panel {
/*
  var pathEdges = ListBuffer[ListBuffer[Byte]]()
  var byteStr = ListBuffer[Byte]()

  var pathsToTilePaths = Map[ListBuffer[Byte], ListBuffer[ListBuffer[Tile]]]()
  var tilePaths = ListBuffer[ListBuffer[Tile]]()
  var currentTilePath = ListBuffer[Tile]()
  var pathSet = Set[Set[Vertex]]()

  var currentPathCoords = List[((Int, Int), (Int, Int))]()
  var currentTileCoords = List[(Int, Int)]()

  var tileToColor = scala.collection.mutable.HashMap[Tile, java.awt.Color]()

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

    g.setBackground(zorn.ivoryBlack)
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
    for ((coord, tile) <- currentTileCoords zip currentTilePath) {
      val x = xPathAxis(coord._1)
      val y = yPathAxis(coord._2)

      g.setColor(tileToColor(tile))
      g.fillRect(x-32, y-32, jump, jump)

      g.setStroke(stroke.four)
      g.setColor(zorn.ivoryBlack)
      g.drawRect(x-32, y-32, jump, jump)

      g.drawString(s"${tile.north}", x-5, y-20)
      g.drawString(s"${tile.east}", x+20, y+5)
      g.drawString(s"${tile.south}", x-5, y+28)
      g.drawString(s"${tile.west}", x-28, y+5)
    }
  }

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
    currentTilePath = tilePaths(tilePathSliderValue)
    repaint()
  }

  def collectTilePaths(alpha: TileSet, hamiltonianPath: Boolean): Unit = {
    val zddRoot = numberLink(vis.grid.graph, vis.h, hamiltonianPath)
    pathEdges = enumZDDValidPaths(zddRoot)
    pathsToTilePaths = mapPathsToTilePaths(vis.h, pathEdges, vis.grid, alpha)

    var totals = 0
    for ((k, v) <- pathsToTilePaths)
      totals += v.length

    println("Totals: "+ totals)

    assert(alpha.tileSet.size <= zorn.blended.length)
    tileToColor.clear()
    tileToColor ++= alpha.tileSet zip zorn.blended.reverse
  }
  */
}
