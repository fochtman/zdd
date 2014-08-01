package com.main

import UnderlyingGraph._
import scala.collection.mutable.{ListBuffer, Set => FrontierSet}

object T1TilePaths {

  trait Adherent {
    def label: Int
    def value: Int
  }

  case class Glue(label: Int) extends Adherent {
    val value = 1
  }

  object nullGlue extends Adherent {
    def label = throw new NoSuchElementException("nullGlue.label")
    def value = throw new NoSuchElementException("nullGlue.value")
    override def toString = "nullGlue"
  }

  object Direction extends Enumeration {
    type Direction = Value
    val NORTH, EAST, SOUTH, WEST = Value
  }
  import Direction._

  trait Square {
    def north: Adherent
    def east: Adherent
    def south: Adherent
    def west: Adherent
  }

  object terminalTile extends Square {
    def north = throw new NoSuchElementException("terminalTile.north")
    def east = throw new NoSuchElementException("terminalTile.east")
    def south = throw new NoSuchElementException("terminalTile.south")
    def west = throw new NoSuchElementException("terminalTile.west")
    override def toString = "termTile"
  }

  case class Tile(north: Adherent, east: Adherent, south: Adherent, west: Adherent) extends Square {

    def glueByEdge(dir: Direction): Adherent = {
      dir match {
        case `NORTH` => this.north
        case `EAST` => this.east
        case `SOUTH` => this.south
        case `WEST` => this.west
      }
    }

    def canBind(that: Tile): Boolean = {
      if (that.north != nullGlue && that.north == this.south) true
      else if (that.south != nullGlue && that.south == this.north) true
      else if (that.east != nullGlue && that.east == this.west) true
      else if (that.west != nullGlue && that.west == this.east) true
      else false
    }

    def canBind(g: Adherent, dir: Direction): Boolean = {
      if (g == nullGlue) false
      else if (glueByEdge(dir) == nullGlue) false
      else if (glueByEdge(dir) == g) true
      else false
    }
  }


  // northBucket = Map[Glue, Set[Tile]]
  // iterate through tiles, grab glue nonNll glue types, put them into set
  // each NESW bucket will have all of the glues as keys
  // the sets for each bucket are built one at a time
  // for the north bucket we want to organize tiles by which have southern glues that can
  // match the key
  case class TileSet(tileSet: Set[Tile]) {
    val glueSet = (this.tileSet map (t => List(t.north, t.east, t.south, t.west))).flatten.toSet - nullGlue

    val northBucket = buildBucket(SOUTH)
    val eastBucket = buildBucket(WEST)
    val southBucket = buildBucket(NORTH)
    val westBucket = buildBucket(EAST)

    def masterBucket(dir: Direction, g: Glue): Set[Square] = {
      dir match {
        case `NORTH` => northBucket(g)
        case `EAST` => eastBucket(g)
        case `SOUTH` => southBucket(g)
        case `WEST` => westBucket(g)
      }
    }

    def buildBucket(dir: Direction) = {
      val bucket = scala.collection.mutable.HashMap[Adherent, Set[Square]]()
      for (g <- glueSet) {
        bucket(g) = Set[Square]()
      }

      val bucketValues =
        for {
          g <- glueSet
          t <- this.tileSet
          if t.canBind(g, dir)
        } yield {
          (g, t)
        }

      for (i <- bucketValues) {
        bucket(i._1) += i._2
      }

      for ((k, v) <- bucket) {
        if (v.isEmpty)
          bucket(k) += terminalTile
      }
      bucket
    }
  }
  def tilePaths(h: List[VertexPair], paths: ListBuffer[ListBuffer[Byte]], g: GridGraph, alpha: TileSet): Unit = {
    val hEdges = g.horizontalEdges
    val vEdges = g.verticalEdges

    val p = paths(0)

    /*
    this section will be in a loop
     */
    val tileFrontier = scala.collection.mutable.HashMap[Int, Set[Tile]](0 -> alpha.tileSet)
    Range(1, p.length) map (i =>
      tileFrontier(i) = Set[Tile]())

    val pathEdgeIndices = (p.zipWithIndex filter (x => x._1 == 1)) map (y => y._2)
    val pathSet = (pathEdgeIndices map (i => g.graph.edges(i)) map (e => Set(e.u, e.v))).toSet
    //println(t)
    //val pathMap = Map(t.indices zip t:_*)

    def directionVector(pathSet: Set[Set[Int]], start: Vertex): Vector[Direction] = {
      def helper(ps: Set[Set[Int]], u: Vertex): Vector[Direction] = {
        val uv = ps.filter(s => s.contains(u))
        assert(uv.size == 1)
        val v = (uv.head - u).head
        edgeDirection(u, v) :: helper(ps - uv.head, v)
      }
      helper(pathSet, start)
    }

    def edgeDirection(u: Vertex, v: Vertex): Direction = {
      
    }


  }


}
