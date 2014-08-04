package com.main

import java.awt.BasicStroke

import com.main.UnderlyingGraph._
import com.main.ZDDMain._
import com.main.T1TilePaths._

import scala.swing.BorderPanel.Position._
import scala.swing.Swing._
import scala.swing.TabbedPane._
import scala.swing._
import scala.swing.event._

//import com.main.BDD.{algoTwo, enumZDDValidPaths2}
//import java.lang.System.{currentTimeMillis => _time}
//import scala.collection.mutable.{HashMap, ListBuffer}
//import scala.swing.GridBagPanel.Fill


object FinalProjUI  extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "fun with zdd"

    contents = new BorderPanel {

      val tabs = new TabbedPane {
        pages += new Page("Parameters", parameters.panel)
        pages += new Page("Grid Graph", gridVis.panel)
        pages += new Page("Grid Graph + Tiles", gridWithTilesVis.panel)
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
}
  object parameters {
    val param = new GridPanel(3, 1) {
      contents += sizes.panel
      contents += algorithms.panel
      contents += buildChoices.panel
    }

    val panel = new BorderPanel {
      layout(param) = Center
      layout(applyParameters.panel) = South
    }

  }

  object sizes {
    val one = new RadioButton("Square Grid Graph")
    val mutex = new ButtonGroup(one)
    mutex.select(one)

    val graphType = new BoxPanel(Orientation.Vertical) {
      contents ++= mutex.buttons
    }

    val h = new ComboBox(1 to 8)
    val w = new ComboBox(1 to 8)

    val hFlow = new FlowPanel {
      contents += h
      contents += new Label{ text = "Rows"}
    }
    val wFlow = new FlowPanel {
      contents += w
      contents += new Label{ text = "Columns"}
    }

    val panel = new BoxPanel(Orientation.Vertical) {
      border = CompoundBorder(TitledBorder(EtchedBorder, "Underlying Graph"), EmptyBorder(5, 5, 5, 10))
      contents += new BorderPanel {
        layout(graphType) = West
      }
      contents += new BorderPanel {
        layout(hFlow) = West
      }
      contents += new BorderPanel {
        layout(wFlow) = West
      }
    }

    def height = () => h.selection.item
    def width = () => w.selection.item
  }

  object algorithms {
    val hamPathEnum = new RadioButton("Hamiltonian Path Enumeration")
    val pathEnum = new RadioButton("Path Enumeration")
    val mutex = new ButtonGroup(hamPathEnum, pathEnum)
    mutex.select(hamPathEnum)

    val panel = new BoxPanel(Orientation.Vertical) {
      border = CompoundBorder(TitledBorder(EtchedBorder, "Algorithm"), EmptyBorder(5, 5, 5, 10))
      contents ++= mutex.buttons
      listenTo(hamPathEnum, pathEnum)
    }

    def getSelection = () => mutex.selected.get.text
  }

  object buildChoices {
    val gridGraph = new RadioButton("Grid Graph")
    val gridGraphTilePaths = new RadioButton("Grid Graph with Tile Paths")
    val dag = new RadioButton("DAG")
    val mutex = new ButtonGroup(gridGraph, gridGraphTilePaths, dag)
    mutex.select(gridGraph)

    val panel = new BoxPanel(Orientation.Vertical) {
      border = CompoundBorder(TitledBorder(EtchedBorder, "Visualize as..."), EmptyBorder(5, 5, 5, 10))
      contents ++= mutex.buttons
      listenTo(gridGraph, gridGraphTilePaths, dag)
    }
  }

  object applyParameters {
    val build = new Button {
      text = "Build"
    }
    val panel = new BoxPanel(Orientation.Vertical) {
      contents += new BorderPanel {
        layout(build) = Center
      }

      listenTo(build)

      reactions += {
        case ButtonClicked(`build`) =>

          buildChoices.mutex.selected match {

            case Some(buildChoices.gridGraph) =>
              System.gc()
              val newHeight = sizes.height() + 1
              val newWidth  = sizes.width() + 1
              vis.updateVis(newHeight, newWidth)
              algorithms.mutex.selected match {
                case Some(algorithms.hamPathEnum) =>
                  gridVis.panel.canvas.collectPathEdges(1)
                case Some(algorithms.pathEnum) =>
                  gridVis.panel.canvas.collectPathEdges(2)
              }
              gridVis.setSlider()

            case Some(buildChoices.gridGraphTilePaths) =>
              System.gc()
              val newHeight = sizes.height() + 1
              val newWidth  = sizes.width() + 1
              vis.updateVis(newHeight, newWidth)
              algorithms.mutex.selected match {
                case Some(algorithms.hamPathEnum) =>
                  gridWithTilesVis.panel.canvas.collectPathEdges(1)
                case Some(algorithms.pathEnum) =>
                  gridWithTilesVis.panel.canvas.collectPathEdges(2)
              }
              gridWithTilesVis.setPathSlider()
              gridWithTilesVis.setTilePathSlider()

            case Some(buildChoices.dag) =>
              algorithms.getSelection() match {
                case "Hamiltonian Path Enumeration" =>
                  DAGVis.pane.canvas.repaintDAG()
                case "Path Enumeration" =>
                  DAGVis.pane.canvas.repaintDAG()
              }
          }
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

  object gridWithTilesVis {
    lazy val panel = new BorderPanel {
      val dim = new Dimension(640, 640)
      val canvas = new GridGraphTilePathCanvas(dim) {
        preferredSize = dim
      }
      val pathSlider = new Slider {
        min = 0
        max = 0
        majorTickSpacing = 1
        paintTicks = true
      }
      val tilePathSlider = new Slider {
        min = 0
        max = 0
        majorTickSpacing = 1
        paintTicks = true
      }

      val sliderPanel = new BorderPanel {
        layout(tilePathSlider) = North
        layout(pathSlider) = South

        listenTo(pathSlider, tilePathSlider)

        reactions += {
          case ValueChanged(`pathSlider`) =>
            if (canvas.pathEdges.length == 1)
              canvas.changePath(0)
            else if (!pathSlider.adjusting && canvas.pathEdges.length != 0)
              canvas.changePath(pathSlider.value)

          case ValueChanged(`tilePathSlider`) =>
            val numCurrentTilePaths = canvas.pathsToTilePaths(canvas.byteStr).size
            if (numCurrentTilePaths == 1)
              canvas.changeTilePath(0)
            else if (!tilePathSlider.adjusting && numCurrentTilePaths != 0)
              canvas.changeTilePath(tilePathSlider.value)
        }
      }

      layout(canvas) = Center
      layout(sliderPanel) = South
    }

    def setPathSlider() = {
      panel.pathSlider.max =
        if (panel.canvas.pathEdges.length == 0)
          0
        else
          panel.canvas.pathEdges.length - 1
      panel.pathSlider.value = 0
    }

    def setTilePathSlider() = {
      val numCurrentTilePaths = panel.canvas.pathsToTilePaths(panel.canvas.byteStr).size
      panel.tilePathSlider.max =
        if (numCurrentTilePaths == 0)
          0
        else
          numCurrentTilePaths - 1
      panel.tilePathSlider.value = 0
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

  /*
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
    val alpha = TileSet(Set(a, b, c, d))

    val hamiltonianPaths = false
    val pathEdges = enumZDDValidPaths(numberLink(vis.grid.graph, vis.h, hamiltonianPaths))

    val m = mapPathsToTilePaths(vis.h, pathEdges, vis.grid, alpha)

    var totals = 0
    for ((k, v) <- m)
      totals += v.length

    println("Totals: "+ totals)

  }
  */
//}

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

