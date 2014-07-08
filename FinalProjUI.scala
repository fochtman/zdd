import java.awt.{ RenderingHints, BasicStroke }
import swing._
import swing.event._
import Swing._
import BorderPanel.Position._

import scala.collection.mutable.ListBuffer

import Graph._
import ZDD._

object FinalProjUI  extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "fun with zdds"

    contents = new BorderPanel {

      val tabs = new TabbedPane {

        import TabbedPane._

        val parameters = new FlowPanel {
          border = Swing.EmptyBorder(5, 5, 5, 5)
          contents += new BoxPanel(Orientation.Vertical) {
            border = CompoundBorder(TitledBorder(EtchedBorder, "Grid Size"), EmptyBorder(5, 5, 5, 10))
            val oneOne = new RadioButton("1 x 1")
            val oneTwo = new RadioButton("1 x 2")
            val oneThree = new RadioButton("1 x 3")
            val twoTwo = new RadioButton("2 x 2")
            val threeThree = new RadioButton("3 x 3")
            val fourFour = new RadioButton("4 x 4")
            val sixSix = new RadioButton("6 x 6")
            val eightEight = new RadioButton("8 x 8")
            val twoFour = new RadioButton("2 x 4")

            val mutex = new ButtonGroup(oneOne, oneTwo, oneThree, twoTwo, threeThree, fourFour, sixSix, eightEight, twoFour)
            contents ++= mutex.buttons
            listenTo(oneOne, oneTwo, oneThree, twoTwo, fourFour, threeThree, sixSix, eightEight, twoFour)
            //val mutex = new ButtonGroup(oneOne, oneTwo, oneThree, twoTwo, threeThree)
            listenTo(oneOne, oneTwo, oneThree, twoTwo, threeThree)

            reactions += {
              case ButtonClicked(`oneOne`) =>
                visualization.canvas.updateGraphDim(2, 2)
              case ButtonClicked(`oneTwo`) =>
                visualization.canvas.updateGraphDim(2, 3)
              case ButtonClicked(`oneThree`) =>
                visualization.canvas.updateGraphDim(2, 4)
              case ButtonClicked(`twoTwo`) =>
                visualization.canvas.updateGraphDim(3, 3)
              case ButtonClicked(`threeThree`) =>
                visualization.canvas.updateGraphDim(4, 4)
              case ButtonClicked(`fourFour`) =>
                visualization.canvas.updateGraphDim(5, 5)
              case ButtonClicked(`sixSix`) =>
                visualization.canvas.updateGraphDim(7, 7)
              case ButtonClicked(`eightEight`) =>
                visualization.canvas.updateGraphDim(9, 9)
              case ButtonClicked(`twoFour`) =>
                visualization.canvas.updateGraphDim(3, 5)
            }
          }

          contents += new BoxPanel(Orientation.Vertical) {
            border = CompoundBorder(TitledBorder(EtchedBorder, "Algorithm"), EmptyBorder(5, 5, 5, 10))
            val one = new RadioButton("one")
            val two = new RadioButton("two")
            val mutex = new ButtonGroup(one, two)

            contents ++= mutex.buttons
            listenTo(one, two)

            reactions += {
              case ButtonClicked(`one`) =>
                visualization.canvas.collectPathEdges(1)
              case ButtonClicked(`two`) =>
                visualization.canvas.collectPathEdges(2)
                visualization.slider.max = visualization.canvas.pathEdges.length - 1
                visualization.slider.value = 0
            }
          }
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
            paintLabels = true
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
  //var pathEdges = List[String]()
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

  val bzl = zorn.blended.length
  val twoStroke = new BasicStroke(2)
  val fourStroke = new BasicStroke(4)
  val eightStroke = new BasicStroke(8)
  val sixteenStroke = new BasicStroke(16)

  override def paintComponent(g: Graphics2D) {
    g.setBackground(zorn.titaniumWhite)
    g.clearRect(0, 0, size.width, size.height)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    g.setStroke(eightStroke)
    g.setColor(zorn.ivoryBlack)
    xAxis map (x =>
      yAxis map (y =>
        g.drawRect(x, y, jump, jump)))

    if (!currentPathCoords.isEmpty) {
      g.setStroke(new BasicStroke(16, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
      g.setColor(zorn.yellowOchreAlpha)
      for ((u, v) <- currentPathCoords) {
        g.drawLine(xPathAxis(u._1), yPathAxis(u._2), xPathAxis(v._1), yPathAxis(v._2))
      }
    }
  }

  def updateGraphDim(m: Int, n: Int): Unit = {
    gridGraph = new GridGraph(m, n)

    xOrigin = (size.width - (jump * (gridGraph.colNum-1))) / 2
    yOrigin = (size.height - (jump * (gridGraph.rowNum-1))) / 2

    xMax = xOrigin + (gridGraph.colNum-1) * jump
    xAxis = (Range(xOrigin, xMax) by jump).toList

    yMax = yOrigin + (gridGraph.rowNum-1) * jump
    yAxis = (Range(yOrigin , yMax) by jump).toList

    xPathAxis = xAxis ::: (xAxis.last + jump :: Nil)
    yPathAxis = yAxis ::: (yAxis.last + jump :: Nil)
  }

  def changePath(sliderValue: Int): Unit = {
    val bStr = pathEdges(sliderValue)

    val pathMap = bStr.zipWithIndex filter (x =>
      x._1 == 1) map (index =>
      gridGraph.graph.edges(index._2))

    currentPathCoords = pathMap.toList map (edge =>
      (gridGraph.vertexToCoord(edge.u), gridGraph.vertexToCoord(edge.v)))

    repaint()
  }

  def collectPathEdges(choice: Int): Unit = choice match {
    case 1 =>
      val zdd = algorithmOne(gridGraph.graph)
      println("Number of paths: "+ countZDDOnePaths(zdd))

    case 2 =>
      val ggV = gridGraph.graph.vertices
      val h = List(VertexPair(ggV(0), ggV.last))
      pathEdges = enumZDDValidPaths(algorithmTwo(gridGraph.graph, h))
      println("Number of valid paths: "+ pathEdges.length)
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
