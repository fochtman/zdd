package com.main

import UnderlyingGraph._
import scala.collection.mutable.ListBuffer

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

  case class Tile(north: Adherent, east: Adherent, south: Adherent, west: Adherent) {

    def matchGlueFrom(dir: Direction): Adherent = {
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
      else if (matchGlueFrom(dir) == nullGlue) false
      else if (matchGlueFrom(dir) == g) true
      else false
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
      for (g <- glueSet) {
        bucket(g) = Set[Tile]()
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

      bucket
    }
  }


  def tilePaths(h: List[VertexPair], paths: ListBuffer[ListBuffer[Byte]], g: GridGraph, alpha: TileSet): Unit = {
    val hEdges = g.horizontalEdges
    val vEdges = g.verticalEdges

    def directionVector(pathSet: Set[Set[Int]], start: Vertex): Vector[Direction] = {
      def helper(ps: Set[Set[Int]], u: Vertex): List[Direction] = {
        val uv = ps.filter(s => s.contains(u))
        assert(uv.size == 1)
        val v = (uv.head - u).head

        if (ps.size == 1)
          edgeDirection(u, v) :: Nil
        else
          edgeDirection(u, v) :: helper(ps - uv.head, v)
      }
      helper(pathSet, start).toVector
    }

    def edgeDirection(v0: Vertex, v1: Vertex): Direction = {
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


    for (p <- paths) {
      val pathEdgeIndices = (p.zipWithIndex filter (x => x._1 == 1)) map (y => y._2)
      val pathSet = (pathEdgeIndices map (i => g.graph.edges(i)) map (e => Set(e.u, e.v))).toSet
      val tileFrontier = scala.collection.mutable.HashMap[Int, Set[Tile]](0 -> alpha.tileSet)
      (1 to pathEdgeIndices.length) map (i =>
        tileFrontier(i) = Set[Tile]())

      val directionForEdge = directionVector(pathSet, h.head.v0)

      for (i <- Range(0, pathEdgeIndices.length); t <- tileFrontier(i)) {
        if (tileFrontier isDefinedAt i + 1) {
          val currentDir = directionForEdge(i)
          val availableGlue = t.matchGlueFrom(currentDir)
          if (availableGlue != nullGlue) {
            val validTiles = alpha.masterBucket(currentDir, availableGlue)
            tileFrontier(i + 1) ++= validTiles
          }
        }
      }
    }
  }


}
