import java.awt.RenderingHints
import UnderlyingGraph._
import ZDDMain._
import T1TilePaths._
import FinalProjUI._
import scala.collection.mutable.ListBuffer
import scala.swing._

object GridGraphCanvas {

  trait CanvasFunctions {
    val jump: Int
    var paths: ListBuffer[ListBuffer[Byte]]
    var currentPath: List[Byte]

    def computeAxis(screenDim: Int, totalAxisUnits: Int): Vector[Int] = {
      val axisLength = (totalAxisUnits - 1) * jump
      val axisOrigin = (screenDim - axisLength) / 2
      val axisMax = axisOrigin + axisLength
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

    def drawGridAndPath(g: Graphics2D, screenWidth: Int, screenHeight: Int): Unit = {
      g.setStroke(stroke.eight)
      g.setColor(zorn.ivoryBlack)

      val xAxis = computeAxis(screenWidth, vis.grid.colNum)
      val yAxis = computeAxis(screenHeight, vis.grid.rowNum)

      xAxis map (x =>
        yAxis map (y =>
          g.drawRect(x, y, jump, jump)))

      if (currentPath.nonEmpty) {
        g.setStroke(stroke.roundSixteen)
        g.setColor(zorn.yellowOchreAlpha)

        val xPathAxis = computePathAxis(xAxis)
        val yPathAxis = computePathAxis(yAxis)
        val pathCoords = computePathCoordinates(currentPath)

        pathCoords map { case (u, v) =>
          g.drawLine(xPathAxis(u._1), yPathAxis(u._2), xPathAxis(v._1), yPathAxis(v._2))
        }
      }
    }

    def collectPaths(hamiltonianPath: Boolean): Unit = {
      val zddRoot: ZDDMain.Node = numberLink(vis.grid.graph, vis.h, hamiltonianPath)
      paths = enumZDDValidPaths(zddRoot)
      if (paths.nonEmpty)
        currentPath = paths(0).toList
      else
        currentPath = List[Byte]()
    }
  }

  class PathCanvas(dim: java.awt.Dimension) extends Panel with CanvasFunctions {

    var paths = ListBuffer[ListBuffer[Byte]]()
    var currentPath = List[Byte]()
    val jump = dim.getHeight.toInt / 10

    override def paintComponent(g: Graphics2D) {
      g.clearRect(0, 0, size.width, size.height)
      g.setBackground(zorn.titaniumWhite)
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      drawGridAndPath(g, size.width, size.height)
    }

    def changePath(sliderValue: Int): Unit = {
      currentPath = paths(sliderValue).toList
      repaint()
    }
  }

  class TilePathCanvas(dim: java.awt.Dimension) extends Panel with CanvasFunctions {

    var paths = ListBuffer[ListBuffer[Byte]]()
    var currentPath = List[Byte]()

    var pathToTilePaths = Map[List[Byte], ListBuffer[ListBuffer[Tile]]]()
    var tilePaths = ListBuffer[ListBuffer[Tile]]()
    var currentTilePath = ListBuffer[Tile]()
    var tileToColor = scala.collection.mutable.HashMap[Tile, java.awt.Color]()

    // the height and width of the grid squares
    val jump = dim.getHeight.toInt / 10

    override def paintComponent(g: Graphics2D) {
      g.setBackground(zorn.blend)
      g.clearRect(0, 0, size.width, size.height)
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      drawGridAndPath(g, size.width, size.height)
      g.setStroke(stroke.four)
      g.setFont(new Font("Ariel", java.awt.Font.ITALIC, 14))
      drawTiles(g, size.width, size.height)
    }

    def computeTilePathCoordinates(path: List[Byte]): List[(Int, Int)] = {
      val pathSet = buildPathSet(path, vis.grid.graph.edges)
      orderedVertexList(pathSet, vis.h.head.v0) map (vertex =>
        vis.grid.vertexToCoord(vertex))
    }

    def drawTiles(g: Graphics2D, screenWidth: Int, screenHeight: Int): Unit = {
      if (currentPath.nonEmpty && currentTilePath.nonEmpty) {
        val xAxis = computeAxis(screenWidth, vis.grid.colNum)
        val yAxis = computeAxis(screenHeight, vis.grid.rowNum)
        val xPathAxis = computePathAxis(xAxis)
        val yPathAxis = computePathAxis(yAxis)
        val currentTileCoords = computeTilePathCoordinates(currentPath)

        for ((coord, tile) <- currentTileCoords zip currentTilePath) {
          val x = xPathAxis(coord._1)
          val y = yPathAxis(coord._2)

          g.setColor(tileToColor(tile))
          g.fillRect(x - 32, y - 32, jump, jump)

          g.setStroke(stroke.four)
          g.setColor(zorn.ivoryBlack)
          g.drawRect(x - 32, y - 32, jump, jump)

          g.drawString(s"${tile.north}", x - 5, y - 20)
          g.drawString(s"${tile.east}", x + 20, y + 5)
          g.drawString(s"${tile.south}", x - 5, y + 28)
          g.drawString(s"${tile.west}", x - 28, y + 5)
        }
      }
    }

    def orderedVertexList(pathSet: Set[Set[Int]], start: Vertex): List[Vertex] = {
      def helper(ps: Set[Set[Int]], u: Vertex): List[Vertex] = {
        val uv = ps.filter(s => s.contains(u))
        assert(uv.size == 1)
        val v = (uv.head - u).head
        if (ps.size == 1)
          v :: Nil
        else
          v :: helper(ps - uv.head, v)
      }
      start :: helper(pathSet, start)
    }

    def changePath(pathSliderValue: Int): Unit = {
      if (paths.length != 0) {
        currentPath = paths(pathSliderValue).toList
        tilePaths = pathToTilePaths(currentPath)
        gridWithTilesVis.setTilePathSlider()
      } else {
        currentPath = List[Byte]()
        tilePaths = ListBuffer[ListBuffer[Tile]]()
      }
      repaint()
    }

    def changeTilePath(tilePathSliderValue: Int): Unit = {
      currentTilePath =
        if (tilePaths.nonEmpty)
          tilePaths(tilePathSliderValue)
        else
          ListBuffer[Tile]()
      repaint()
    }

    def buildTilePaths(alpha: TileSet): Unit = {
      pathToTilePaths = mapPathsToTilePaths(vis.h, paths, vis.grid, alpha)

      if (pathToTilePaths.contains(currentPath))
        tilePaths = pathToTilePaths(currentPath)
      else
        tilePaths = ListBuffer[ListBuffer[Tile]]()

      var totals = 0
      for ((k, v) <- pathToTilePaths)
        totals += v.length

      println("Totals: "+ totals)

      assert(alpha.tileSet.size <= zorn.blended.length)
      tileToColor.clear()
      tileToColor ++= alpha.tileSet zip zorn.blended.distinct.reverse
    }
  }
}
