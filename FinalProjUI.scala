import java.awt.{BorderLayout, RenderingHints, BasicStroke}
import swing._
import swing.event._
import Swing._
import BorderPanel.Position._
import TabbedPane._

import scala.collection.mutable.ListBuffer
import System.{currentTimeMillis => _time}

import Graph._
import ZDD._
import BDD.{algoTwo, enumZDDValidPaths2}


object FinalProjUI  extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "fun with zdds"

    contents = new BorderPanel {

      val tabs = new TabbedPane {

        val param = new GridPanel(3, 1) {

          val height = new ComboBox(1 to 8)
          val width = new ComboBox(1 to 8)
          val hFlow = new FlowPanel{ contents += new Label("Rows: ");   contents += height }
          val wFlow = new FlowPanel{ contents += new Label("Columns: "); contents += width }
          val gridSizeChoices = new BoxPanel(Orientation.Vertical) {
            border = CompoundBorder(TitledBorder(EtchedBorder, "Rows * Columns"), EmptyBorder(5, 5, 5, 10))
            contents += hFlow
            contents += wFlow
          }
          contents += gridSizeChoices

          val one = new RadioButton("one")
          val two = new RadioButton("Numberlink Solver")
          val mutex = new ButtonGroup(one, two)
          val algorithmSelection = new BoxPanel(Orientation.Vertical) {
            border = CompoundBorder(TitledBorder(EtchedBorder, "Algorithm"), EmptyBorder(5, 5, 5, 10))
            contents ++= mutex.buttons
            listenTo(one, two)
          }
          contents += algorithmSelection

          val applyGridSizeAndAlgorithm = new Button { text = "Apply" }
          val applyPanel = new BorderPanel { layout(applyGridSizeAndAlgorithm) = North }
          contents += applyPanel

          listenTo(applyGridSizeAndAlgorithm)
          reactions += {
            case ButtonClicked(`applyGridSizeAndAlgorithm`) =>
              /*
              The ZDDs can get rather large (in the millions of nodes) so it is pertinent
              to clean the memory as best as we can. So we call System.gc()
               */
              System.gc()
              var newHeight = height.selection.item + 1
              var newWidth  = width.selection.item + 1
              visualization.canvas.updateGraphDim(newHeight, newWidth)
              if (mutex.selected.get.text == "one")
                visualization.canvas.collectPathEdges(1)
              else if (mutex.selected.get.text == "Numberlink Solver")
                visualization.canvas.collectPathEdges(2)

              visualization.slider.max = visualization.canvas.pathEdges.length - 1
              visualization.slider.value = 0
          }
        }

        val parameters = new BorderPanel {
          layout(param) = Center
        }
        pages += new Page("Parameters", parameters)

        lazy val visualization = new BorderPanel {
          val canvas = new Canvas {
            preferredSize = new Dimension(640, 640)
          }
          object slider extends Slider {
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
        pages += new Page("Visualization", visualization)
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

class Canvas extends Panel {
  var pathEdges = ListBuffer[ListBuffer[Byte]]()
  var currentPathCoords = List[((Int, Int), (Int, Int))]()

  private val jump = 64

  // perhaps make this apart of a companion object to the canvas class
  private var gridGraph = new GridGraph(2, 2)

  // Hack Alert by way of the 640 constant.
  // center gridGraph
  private var xOrigin = (640 - (jump * gridGraph.colNum-1)) / 2
  private var yOrigin = (640 - (jump * gridGraph.rowNum-1)) / 2

  private var xMax = xOrigin + (gridGraph.colNum-1) * jump
  private var xAxis = (Range(xOrigin, xMax) by jump).toList

  private var yMax = yOrigin + (gridGraph.rowNum-1) * jump
  private var yAxis = (Range(yOrigin , yMax) by jump).toList
  /*
  The xAxis and yAxis contain points which correspond the to top
  left corner of each square in the grid. So when drawing the paths
  over the grid, we need one extra point at the end of each axis list.
   */
  private var xPathAxis = xAxis ::: (xAxis.last + 64 :: Nil)
  private var yPathAxis = yAxis ::: (yAxis.last + 64 :: Nil)

  val eightStroke = new BasicStroke(8)
  val sixteenStroke = new BasicStroke(16)
  val roundSixteenStroke = new BasicStroke(16, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)

  override def paintComponent(g: Graphics2D) {
    g.setBackground(zorn.titaniumWhite)
    g.clearRect(0, 0, size.width, size.height)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    g.setStroke(eightStroke)
    g.setColor(zorn.ivoryBlack)

    xAxis map (x =>
      yAxis map (y =>
        g.drawRect(x, y, jump, jump)))

    g.setStroke(roundSixteenStroke)
    g.setColor(zorn.yellowOchreAlpha)

    currentPathCoords map { case (u, v) =>
      g.drawLine(xPathAxis(u._1), yPathAxis(u._2),
                 xPathAxis(v._1), yPathAxis(v._2))
    }
  }

  def updateGraphDim(m: Int, n: Int): Unit = {
    gridGraph = new GridGraph(m, n)

    xOrigin = (size.width - (jump * (gridGraph.colNum-1))) / 2
    yOrigin = (size.height - (jump * (gridGraph.rowNum-1))) / 2

    xMax = xOrigin + (gridGraph.colNum - 1) * jump
    xAxis = (Range(xOrigin, xMax) by jump).toList

    yMax = yOrigin + (gridGraph.rowNum - 1) * jump
    yAxis = (Range(yOrigin , yMax) by jump).toList

    xPathAxis = xAxis ::: (xAxis.last + jump) :: Nil
    yPathAxis = yAxis ::: (yAxis.last + jump) :: Nil
  }

  def changePath(sliderValue: Int): Unit = {
    val byteStr = pathEdges(sliderValue)

    val pathMap = byteStr.zipWithIndex filter (x =>
      x._1 == 1) map (index =>
        gridGraph.graph.edges(index._2))

    currentPathCoords = pathMap.toList map (edge =>
      (gridGraph.vertexToCoord(edge.u), gridGraph.vertexToCoord(edge.v)))

    repaint()
  }

  def time[R](block: => R, funcName: String): R = {
    val t0 = _time
    val result = block
    val t1 = _time
    println(funcName +"\telapsed time: " + (t1 - t0) + "ms")
    result
  }

  def collectPathEdges(choice: Int): Unit = choice match {
    case 1 =>
      val ggV = gridGraph.graph.vertices
      val h = List(VertexPair(ggV(0), ggV.last))
      val zdd = time (algorithmTwo(gridGraph.graph, h), "Algo2 =>")
      pathEdges = time (enumZDDValidPaths(zdd), "Path finding =>\t")
      println("Algo2 Number of valid paths: "+ pathEdges.length +"\n")

    case 2 =>
      val ggV = gridGraph.graph.vertices
      val h = List(VertexPair(ggV(0), ggV.last))
      val zdd = time (algoTwo(gridGraph.graph, h), "Numberlink solver =>")
      pathEdges = time (enumZDDValidPaths2(zdd), "Path finding =>\t")
      println("Number of valid paths: "+ pathEdges.length +"\n")

  }
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
