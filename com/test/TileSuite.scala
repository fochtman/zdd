package com.test

import com.main.T1TilePaths
import T1TilePaths._
import T1TilePaths.Direction._

import org.scalatest.{GivenWhenThen, FeatureSpec}

class TileSuite extends FeatureSpec {

  val sq = T1TilePaths.Glue('a'.toInt)
  val wt = T1TilePaths.Glue('b'.toInt)
  val dt = T1TilePaths.Glue('c'.toInt)
  val ci = T1TilePaths.Glue('d'.toInt)
  val cc = T1TilePaths.Glue('e'.toInt)

  val a = Tile(nullGlue, nullGlue, sq, wt)
  val b = Tile(sq, ci, nullGlue, nullGlue)
  val c = Tile(dt, nullGlue, nullGlue, ci)
  val d = Tile(nullGlue, wt, dt, nullGlue)

  val alpha = TileSet(Set(a, b, c, d))

  feature("(a: tile)canBind(b: tile)") {
    assert(a.canBind(b))
    assert(b.canBind(c))
    assert(c.canBind(d))
    assert(d.canBind(a))

    assert(!d.canBind(b))
  }

  feature("(a: tile)canBind(g: Glue, d: direction)") {
    assert(a.canBind(sq, SOUTH))
    assert(!a.canBind(wt, SOUTH))
  }

  feature("TileSet tests") {
    assert(alpha.glueSet == Set(sq, wt, dt, ci))
    // test buckets
  }
}
