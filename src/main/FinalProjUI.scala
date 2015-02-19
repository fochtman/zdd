import UnderlyingGraph._
import GridGraphCanvas._
import T1TilePaths._
import java.awt.BasicStroke
import scala.swing.BorderPanel.Position._
import scala.swing.Swing._
import scala.swing.TabbedPane._
import scala.swing._
import scala.swing.event._

object FinalProjUI extends SimpleSwingApplication {
  def top = new MainFrame {
    title = ""

    contents = new BorderPanel {

      val tabs = new TabbedPane {
        pages += new Page("Parameters", parameters.panel)
        pages += new Page("Grid Graph", gridVis.panel)
        pages += new Page("Grid Graph + Tiles", gridWithTilesVis.panel)
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

    val graphType = new BoxPanel(Orientation.Vertical) { contents ++= mutex.buttons }

    val h = new ComboBox(1 to 8)
    val hFlow = new FlowPanel {
      contents += h
      contents += new Label {
        text = "Rows"
      }
    }

    val w = new ComboBox(1 to 8)
    val wFlow = new FlowPanel {
      contents += w
      contents += new Label {
        text = "Columns"
      }
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

    def height = () => h.selection.item + 1
    def width = () => w.selection.item + 1
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
    val mutex = new ButtonGroup(gridGraph, gridGraphTilePaths)
    mutex.select(gridGraph)

    val panel = new BoxPanel(Orientation.Vertical) {
      border = CompoundBorder(TitledBorder(EtchedBorder, "Visualize as..."), EmptyBorder(5, 5, 5, 10))
      contents ++= mutex.buttons
      listenTo(gridGraph, gridGraphTilePaths)
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
          val hamiltonianPath =
            algorithms.mutex.selected match {
              case Some(algorithms.hamPathEnum) =>
                true
              case Some(algorithms.pathEnum) =>
                false
              case _ => throw new NoSuchElementException
            }
          vis.updateVis(sizes.height(), sizes.width())

          buildChoices.mutex.selected match {

            case Some(buildChoices.gridGraph) =>
              System.gc()
              gridVis.panel.canvas.collectPaths(hamiltonianPath)
              gridVis.panel.canvas.setSlider()

            case Some(buildChoices.gridGraphTilePaths) =>
              System.gc()
              gridWithTilesVis.panel.canvas.collectPaths(hamiltonianPath)
              gridWithTilesVis.panel.canvas.buildTilePaths(tileSets.beta)
              gridWithTilesVis.setPathSlider()
              gridWithTilesVis.setTilePathSlider()

            case _ => throw new NoSuchElementException
          }
      }
    }
  }

  object gridVis {
    lazy val panel = new BorderPanel {
      val dim = new Dimension(640, 640)
      val canvas = new PathCanvas(dim) {
        preferredSize = dim
        val slider = new Slider {
          min = 0
          max = 0
          majorTickSpacing = 1
          paintTicks = true
        }

        listenTo(slider)
        reactions += {
          case ValueChanged(`slider`) =>
            if (!slider.adjusting && paths.length != 0)
              changePath(slider.value)
        }

        def setSlider(): Unit = {
          slider.value = 0
          slider.max =
            if (paths.length >= 1)
              paths.length - 1
            else
              0
          if (paths.length == 1)
            changePath(0)
        }
      }
      layout(canvas) = Center
      layout(canvas.slider) = South
    }
  }

  object gridWithTilesVis {
    lazy val panel = new BorderPanel {
      val dim = new Dimension(640, 640)
      var canvas = new TilePathCanvas(dim) {
        preferredSize = dim
      }
      val pathSlider = new Slider {
        min = 0
        max = 0
        value = -1
        majorTickSpacing = 1
        paintTicks = true
      }
      val tilePathSlider = new Slider {
        min = 0
        max = 0
        value = -1
        majorTickSpacing = 1
        paintTicks = true
      }
      val sliderPanel = new BorderPanel {
        layout(tilePathSlider) = North
        layout(pathSlider) = South

        listenTo(pathSlider, tilePathSlider)
        reactions += {
          case ValueChanged(`pathSlider`) =>
            if (!pathSlider.adjusting && canvas.paths.length != 0)
              canvas.changePath(pathSlider.value)

          case ValueChanged(`tilePathSlider`) =>
            if (!tilePathSlider.adjusting)
              canvas.changeTilePath(tilePathSlider.value)
        }
      }
      layout(canvas) = Center
      layout(sliderPanel) = South
    }

    def setPathSlider(): Unit = {
      panel.pathSlider.value = 0
      panel.pathSlider.max =
        if (panel.canvas.paths.length >= 1)
          panel.canvas.paths.length - 1
        else
          0
    }

    def setTilePathSlider(): Unit = {
      val currPath = panel.canvas.currentPath
      if (panel.canvas.pathToTilePaths.contains(currPath)) {
        val tilePathLengths = panel.canvas.pathToTilePaths(currPath).length
        if (tilePathLengths == 0 || tilePathLengths == 1) {
          panel.canvas.changeTilePath(0)
          panel.tilePathSlider.value = 0
          panel.tilePathSlider.max = 0
        } else {
          panel.tilePathSlider.value = 0
          panel.tilePathSlider.max = tilePathLengths - 1
        }
      } else {
        panel.tilePathSlider.value = 0
        panel.tilePathSlider.max = 0
      }
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

  object tileSets {
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
    val g = Tile(sq, cc, sq, cc)
    val alpha = TileSet(Set(a, b, c, d, e, f, g))
    val beta = TileSet(Set(a, b, c, d, f, g))
    val theta = TileSet(Set(e, f, g))
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
    val ivoryBlackAlpha = new Color(40, 36, 34, 128)

    val blend = new Color(181, 117, 90, 150) // more or less brownish
    val gray = new Color(128, 128, 128)
    val YBW = List[Color](yellowOchre, ivoryBlack, titaniumWhite)
    val palette = List[Color](yellowOchre, cadmiumRedMedium, ivoryBlack, titaniumWhite)
    val paletteAlpha = List[Color](yellowOchreAlpha, cadmiumRedMediumAlpha, ivoryBlackAlpha, titaniumWhiteAlpha)
    val paletteRand = randomPalette
    val blended = blendPalette(palette)

    def yellow(a: Int) = new Color(245, 197, 44, a)
    def red(a: Int) = new Color(196, 1, 45, a)
    def green(a: Int) = new Color(45, 196, 1, a)
    def blue(a: Int) = new Color(1, 45, 196, a)

    def randomPalette: List[Color] = {
      import scala.util.Random
      val colorFuncs = List(yellow(_), red(_), green(_), blue(_))
      val sixteenColors =
        for {
          cf <- colorFuncs
          j <- (1 to 4).toList
          color = cf(Random.nextInt(195) + 60)
        } yield color
      Random.shuffle(sixteenColors)
    }

    def blendPalette(palette: List[Color]): List[Color] = {
      def f(c1: Int, c2: Int) = (c1+c2)/2
      for {
        i <- palette
        j <- palette
      } yield {
        new Color(f(i.getRed, j.getRed), f(i.getGreen, j.getGreen), f(i.getBlue, j.getBlue), 230)
      }
    }
  }
}
