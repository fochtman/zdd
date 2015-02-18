import UnderlyingGraph._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

object T1TilePaths {

  trait Adherent {
    def label: Int
    def value: Int
  }

  case class Glue(label: Int) extends Adherent {
    val value = 1
    override def toString = s"${label.toChar}"
  }

  object nullGlue extends Adherent {
    def label = throw new NoSuchElementException("nullGlue.label")
    def value = throw new NoSuchElementException("nullGlue.value")
    override def toString = ""
  }

  object Direction extends Enumeration {
    type Direction = Value
    val NORTH, EAST, SOUTH, WEST = Value
  }
  import Direction._

  case class Tile(north: Adherent, east: Adherent, south: Adherent, west: Adherent) {

    def matchGlueFrom(dir: Direction): Adherent = {
      dir match {
        case `NORTH` => this.north
        case `EAST` => this.east
        case `SOUTH` => this.south
        case `WEST` => this.west
      }
    }

    def oppositeEdge(dir: Direction): Adherent = {
      dir match {
        case `NORTH` => this.south
        case `EAST` => this.west
        case `SOUTH` => this.north
        case `WEST` => this.east
      }
    }

    def canBind(that: Tile): Boolean = {
      if (that.north != nullGlue && that.north == this.south)
        true
      else if (that.south != nullGlue && that.south == this.north)
        true
      else if (that.east != nullGlue && that.east == this.west)
        true
      else if (that.west != nullGlue && that.west == this.east)
        true
      else
        false
    }

    def canBind(g: Adherent, dir: Direction): Boolean = {
      if (g == nullGlue)
        false
      else if (this.matchGlueFrom(dir) == nullGlue)
        false
      else if (this.matchGlueFrom(dir) == g)
        true
      else
        false
    }

    def canBind(that: Tile, dir: Direction): Boolean = {
      val currentEdge = this.oppositeEdge(dir)
      val proceedingEdge = that.matchGlueFrom(dir)
      if (currentEdge != nullGlue && proceedingEdge == currentEdge)
        true
      else
        false
    }
  }

  case class TileSet(tileSet: Set[Tile]) {
    val glueSet = (this.tileSet map (t => List(t.north, t.east, t.south, t.west))).flatten.toSet - nullGlue

    val northBucket = buildBucket(SOUTH)
    val eastBucket = buildBucket(WEST)
    val southBucket = buildBucket(NORTH)
    val westBucket = buildBucket(EAST)


    def masterBucket(dir: Direction, g: Adherent): Set[Tile] = {
      dir match {
        case `NORTH` => northBucket(g)
        case `EAST` => eastBucket(g)
        case `SOUTH` => southBucket(g)
        case `WEST` => westBucket(g)
      }
    }

    def buildBucket(dir: Direction) = {
      val bucket = scala.collection.mutable.HashMap[Adherent, Set[Tile]]()

      glueSet map (g =>
        bucket(g) = Set[Tile]())

      val bucketValues =
        for {
          g <- glueSet
          t <- this.tileSet
          if t.canBind(g, dir)
        } yield {
          (g, t)
        }

      bucketValues map (i =>
        bucket(i._1) += i._2)

      bucket
    }
  }

  def edgeDirection(v0: Vertex, v1: Vertex, g: GridGraph): Direction = {
    val hEdges = g.horizontalEdges
    val vEdges = g.verticalEdges

    val (edge, uv) =
      if (v0 < v1)
        (Edge(v0, v1), true)
      else
        (Edge(v1, v0), false)

    if (vEdges.contains(edge) && !uv) NORTH
    else if (hEdges.contains(edge) && uv) EAST
    else if (vEdges.contains(edge) && uv) SOUTH
    else if (hEdges.contains(edge) && !uv) WEST
    else throw new NoSuchElementException("Edge direction malfunction")
  }

  def directionVector(pathSet: Set[Set[Int]], start: Vertex, g: GridGraph): Vector[Direction] = {

    def helper(ps: Set[Set[Int]], u: Vertex): List[Direction] = {
      val uv = ps.filter(s => s.contains(u))
      assert(uv.size == 1)
      val v = (uv.head - u).head

      if (ps.size == 1)
        edgeDirection(u, v, g) :: Nil
      else
        edgeDirection(u, v, g) :: helper(ps - uv.head, v)
    }

    helper(pathSet, start).toVector
  }

  def buildPathSet(path: List[Byte], edges: List[Edge]): Set[Set[Vertex]] = {
    val pathEdgeIndices =
      (path.zipWithIndex filter (x =>
        x._1 == 1)) map (y => y._2)

    val pathSet =
      (pathEdgeIndices map (i =>
        edges(i)) map (e =>
        Set(e.u, e.v))).toSet

    pathSet
  }

  def buildTileFrontier(directions: Vector[Direction], alpha: TileSet): HashMap[Int, Set[Tile]] = {
    val tileFrontier = HashMap[Int, Set[Tile]](0 -> alpha.tileSet)
    val pathSize = directions.length
    (1 to pathSize) map (i =>
      tileFrontier(i) = Set[Tile]())

    for (i <- Range(0, pathSize); t <- tileFrontier(i)) {
      if (tileFrontier isDefinedAt i + 1) {
        val currentDir = directions(i)
        val availableGlue = t.matchGlueFrom(currentDir)
        if (availableGlue != nullGlue) {
          val validTiles = alpha.masterBucket(currentDir, availableGlue)
          tileFrontier(i + 1) ++= validTiles
        }
      }
    }
    tileFrontier
  }

  def tilePaths(directions: Vector[Direction], tileFrontier: HashMap[Int, Set[Tile]]): ListBuffer[ListBuffer[Tile]] = {
    // reverse sort the tileFrontier keys, so that we can start building our tile paths from the bottom of the tree up
    val last = tileFrontier.keys.toList.sortWith(_ > _).head
    val tilePaths = ListBuffer[ListBuffer[Tile]]()

    def helper(i: Int, currentPath: ListBuffer[Tile]): Unit = {
      if (tileFrontier isDefinedAt i - 1) {
        val pathHeadTile = currentPath.head
        for (t <- tileFrontier(i-1))
          if (pathHeadTile.canBind(t, directions(i-1)))
            helper(i - 1, t +: currentPath)
      } else
        currentPath +=: tilePaths
    }

    tileFrontier(last) map (t =>
      helper(last, ListBuffer[Tile](t)))

    tilePaths
  }

  def mapPathsToTilePaths(h: List[VertexPair], paths: ListBuffer[ListBuffer[Byte]], g: GridGraph, alpha: TileSet): Map[List[Byte], ListBuffer[ListBuffer[Tile]]] = {
    val edges = g.graph.edges
    val startVertex = h.head.v0

    val tilePathsList =
      for (p <- paths) yield {
        val pathSet = buildPathSet(p.toList, edges)
        val directions = directionVector(pathSet, startVertex, g)
        val tileFrontier = buildTileFrontier(directions, alpha)
        tilePaths(directions, tileFrontier)
      }
    val pathsWithInnerList = paths map (_.toList)
    Map(pathsWithInnerList zip tilePathsList:_*)
  }
}
