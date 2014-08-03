package com.main

import java.awt.{BasicStroke, RenderingHints}
import java.lang.System.{currentTimeMillis => _time}

import com.main.UnderlyingGraph._
import com.main.ZDDMain._
import com.main.T1TilePaths._
import com.main.BDD.{algoTwo, enumZDDValidPaths2}

import scala.collection.mutable.{HashMap, ListBuffer}
import scala.swing.BorderPanel.Position._
import scala.swing.Swing._
import scala.swing.TabbedPane._
import scala.swing._
import scala.swing.event._


object FinalProjUI  extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "fun with zdd"

    contents = new BorderPanel {

      val tabs = new TabbedPane {

        val param = new GridPanel(3, 1) {

          contents += sizes.panel
          contents += algorithms.panel
          contents += buildChoices.panel

          listenTo(buildChoices.Grid)
          listenTo(buildChoices.DAG)
          listenTo(buildChoices.TileLink)

          reactions += {
            case ButtonClicked(buildChoices.Grid) =>
              System.gc()
              val newHeight = sizes.height() + 1
              val newWidth  = sizes.width() + 1
              algorithms.getSelection() match {
                case "Path Enumeration" =>
                  vis.updateVis(newHeight, newWidth)
                  gridVis.panel.canvas.collectPathEdges(1)
                case "Numberlink Solver" =>
                  vis.updateVis(newHeight, newWidth)
                  gridVis.panel.canvas.collectPathEdges(2)
              }

              gridVis.setSlider()

            case ButtonClicked(buildChoices.DAG) =>
              algorithms.getSelection() match {
                case "Path Enumeration" =>
                  DAGVis.pane.canvas.repaintDAG()
                case "Numberlink Solver" =>
                  DAGVis.pane.canvas.repaintDAG()
              }

            case ButtonClicked(buildChoices.TileLink) =>
              System.gc()
              val newHeight = sizes.height() + 1
              val newWidth  = sizes.width() + 1
              vis.updateVis(newHeight, newWidth)
              tmpTileLink()
          }
        }

        val parameters = new BorderPanel {
          layout(param) = Center
        }
        pages += new Page("Parameters", parameters)

        pages += new Page("Grid Graph", gridVis.panel)

        pages += new Page("DAG", DAGVis.pane)

      }
      layout(tabs) = Center
    }

    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(Action("Exit") {
          sys.exit(0)
        })
      }
    }
  }

  object gridVis {
    lazy val panel = new BorderPanel {
      val dim = new Dimension(640, 640)
      val canvas = new GridGraphCanvas(dim) {
        preferredSize = dim
      }
      val slider = new Slider {
        min = 0
        max = 0
        majorTickSpacing = 1
        paintTicks = true
      }

      layout(canvas) = Center
      layout(slider) = South
      listenTo(slider)

      reactions += {
        case ValueChanged(`slider`) =>
          if (canvas.pathEdges.length == 1)
            canvas.changePath(0)
          else if (!slider.adjusting && canvas.pathEdges.length != 0)
            canvas.changePath(slider.value)
      }
    }

    def setSlider() = {
      panel.slider.max =
        if (panel.canvas.pathEdges.length == 0)
          0
        else
          panel.canvas.pathEdges.length - 1
      panel.slider.value = 0
    }
  }

  object sizes {
    val h = new ComboBox(1 to 8)
    val w = new ComboBox(1 to 8)
    val hFlow = new FlowPanel {
      contents += h
      contents += new Label("Rows")
    }
    val wFlow = new FlowPanel {
      contents += w
      contents += new Label("Columns")
    }
    val panel = new BoxPanel(Orientation.Vertical) {
      border = CompoundBorder(TitledBorder(EtchedBorder, "Rows * Columns"), EmptyBorder(5, 5, 5, 10))
      contents += hFlow
      contents += wFlow
    }

    def height = () => h.selection.item
    def width = () => w.selection.item
  }

  object algorithms {
    val one = new RadioButton("Path Enumeration")
    val two = new RadioButton("Numberlink Solver")
    val mutex = new ButtonGroup(one, two)

    val panel = new BoxPanel(Orientation.Vertical) {
      border = CompoundBorder(TitledBorder(EtchedBorder, "Algorithm"), EmptyBorder(5, 5, 5, 10))
      contents ++= mutex.buttons
      listenTo(one, two)
    }

    def getSelection = () => mutex.selected.get.text
  }

  object buildChoices {
    val Grid = new Button {
      text = "Grid"
    }
    val DAG = new Button {
      text = "DAG"
    }
    val TileLink = new Button {
      text = "TileLink"
    }
    val panel = new BoxPanel(Orientation.Vertical) {
      border = CompoundBorder(TitledBorder(EtchedBorder, "View ZDD on a... "), EmptyBorder(5, 5, 5, 10))
      contents += Grid
      contents += DAG
      contents += TileLink
    }
  }

  object DAGVis {
    lazy val pane = new ScrollPane {
      val canvas = new DAGCanvas {
        //preferredSize = new Dimension(640, 640)
      }
      contents = canvas
    }
  }

  def tmpTileLink(): Unit = {
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
    //val alpha = TileSet(Set(a, b, c, d, e, f))
    val alpha = TileSet(Set(a, b, c))

    val pathEdges = enumZDDValidPaths(numberLink(vis.grid.graph, vis.h))

    val m = mapPathToTilePaths(vis.h, pathEdges, vis.grid, alpha)

    m.foreach(println)

  }
}

object vis {
  var grid = GridGraph(2, 2)

  var ggV: List[UnderlyingGraph.Vertex] = grid.graph.vertices
  var h = List(VertexPair(ggV(0), ggV.last))
  var root: ZDDMain.Node = null

  def updateVis(height: Int, width: Int) {
    grid = GridGraph(height, width)
    ggV = grid.graph.vertices
    h = List(VertexPair(ggV(0), ggV.last))
  }
}

object stroke {
  val four = new BasicStroke(4)
  val strokePattern = Array(8.0f)
  val fourDashed = new BasicStroke(4, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 1.0f, strokePattern, 0)
  val eight = new BasicStroke(8)
  val sixteen = new BasicStroke(16)
  val roundSixteen = new BasicStroke(16, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)
}

object zorn  {
  val yellowOchre = new Color(245, 197, 44)
  val cadmiumRedMedium = new Color(196, 1, 45)
  val ivoryBlack = new Color(40, 36, 34)
  val titaniumWhite = new Color(244, 237, 237)

  val yellowOchreAlpha = new Color(245, 197, 44, 128)
  val cadmiumRedMediumAlpha = new Color(196, 1, 45, 128)
  val titaniumWhiteAlpha = new Color(244, 237, 237, 64)

  val blend = new Color(181, 117, 90, 150)
  val YBW = List[Color](yellowOchre, ivoryBlack, titaniumWhite)
  val palette = List[Color](yellowOchre, cadmiumRedMedium, ivoryBlack, titaniumWhite)
  val blended = blendPalette(palette)

  def blendPalette(palette: List[Color]): List[Color] = {
    def f(c1: Int, c2: Int) = (c1+c2)/2
    for (i <- palette; j <- palette) yield {
      new Color(f(i.getRed, j.getRed), f(i.getGreen, j.getGreen), f(i.getBlue, j.getBlue), 30)
    }
  }
}

/*
case Node(_, _, `zeroTerminal`, hi: Node) =>
  npl(i) += 1
  helper(hi, i+1)

case Node(_, _, `oneTerminal`, hi: Node) =>
  npl(i) += 1
  helper(hi, i+1)

case Node(_, _, lo: Node, `zeroTerminal`) =>
  npl(i) += 1
  helper(lo, i+1)

case Node(_, _, lo: Node, `oneTerminal`) =>
  npl(i) += 1
  helper(lo, i+1)

case Node(_, _, `zeroTerminal`, `zeroTerminal`) =>
  npl(i) += 1

case Node(_, _, null, null) =>
  npl(i) += 1

case Node(_, _, `zeroTerminal`, `oneTerminal`) =>
  npl(i) += 1

case Node(_, _, `oneTerminal`, `zeroTerminal`) =>
  npl(i) += 1

case Node(_, _, `oneTerminal`, `oneTerminal`) =>
  npl(i) += 1
*/
