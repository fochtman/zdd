import java.awt.{RenderingHints, BasicStroke}
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

          val buildGrid = new Button { text = "Grid" }
          val buildDAG = new Button { text = "DAG" }
          val buildChoices = new BoxPanel(Orientation.Vertical) {
            border = CompoundBorder(TitledBorder(EtchedBorder, "View ZDD on a... "), EmptyBorder(5, 5, 5, 10))
            contents += buildGrid
            contents += buildDAG
          }
          contents += buildChoices

          listenTo(buildGrid)
          listenTo(buildDAG)
          reactions += {
            case ButtonClicked(`buildGrid`) =>
              /*
              The ZDDs can get large (in the millions of nodes once 5 x 5 and up is selected)
              to clean the memory as best as we can call System.gc()
               */
              System.gc()

              var newHeight = height.selection.item + 1
              var newWidth  = width.selection.item + 1
              gridVis.canvas.gridGraph = GridGraph(newHeight, newWidth)

              mutex.selected.get.text match {
                case "one" =>
                  gridVis.canvas.collectPathEdges(1)
                case "Numberlink Solver" =>
                  gridVis.canvas.collectPathEdges(2)
              }

              gridVis.slider.max = gridVis.canvas.pathEdges.length - 1
              gridVis.slider.value = 0

            case ButtonClicked(`buildDAG`) =>
              mutex.selected.get.text match {
                case "one" =>
                  DAGVis.canvas.repaintDAG()
                case "Numberlink Solver" =>
                  DAGVis.canvas.repaintDAG()
              }

          }
        }

        val parameters = new BorderPanel {
          layout(param) = Center
        }
        pages += new Page("Parameters", parameters)

        lazy val gridVis = new BorderPanel {
          val dim = new Dimension(640, 640)
          val canvas = new GridGraphCanvas(dim) {
            preferredSize = dim
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
        pages += new Page("Grid Graph", gridVis)

        lazy val DAGVis = new ScrollPane {
          val canvas = new DAGCanvas {
            //preferredSize = new Dimension(640, 640)
          }
          contents = canvas
        }
        pages += new Page("DAG", DAGVis)

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

object ZDDBuilder {

}

class GridGraphCanvas(dim: java.awt.Dimension) extends Panel {

  var pathEdges = ListBuffer[ListBuffer[Byte]]()
  var currentPathCoords = List[((Int, Int), (Int, Int))]()

  private var currentCanvasHeight = dim.getHeight.toInt
  private var currentCanvasWidth = dim.getWidth.toInt

  // the height and width of the grid squares
  private val jump = currentCanvasHeight / 10

  var gridGraph = new GridGraph(2, 2)

  private var xOrigin = (currentCanvasWidth - (jump * gridGraph.colNum-1)) / 2
  private var yOrigin = (currentCanvasHeight - (jump * gridGraph.rowNum-1)) / 2

  private var xMax = xOrigin + (gridGraph.colNum-1) * jump
  private var xAxis = (Range(xOrigin, xMax) by jump).toList

  private var yMax = yOrigin + (gridGraph.rowNum-1) * jump
  private var yAxis = (Range(yOrigin , yMax) by jump).toList
  /*
  The xAxis and yAxis contain points which correspond the to top
  left corner of each square in the grid. So when drawing the paths
  over the grid, we need one extra point at the end of each axis list.
   */
  private var xPathAxis = xAxis ::: (xAxis.last + jump :: Nil)
  private var yPathAxis = yAxis ::: (yAxis.last + jump :: Nil)

  def updateGraphCanvasFields(): Unit = {
    if (currentCanvasHeight != size.height || currentCanvasWidth != size.width) {
      currentCanvasHeight = size.height
      currentCanvasWidth = size.width

      xOrigin = (size.width - (jump * (gridGraph.colNum - 1))) / 2
      yOrigin = (size.height - (jump * (gridGraph.rowNum - 1))) / 2

      xMax = xOrigin + (gridGraph.colNum - 1) * jump
      xAxis = (Range(xOrigin, xMax) by jump).toList

      yMax = yOrigin + (gridGraph.rowNum - 1) * jump
      yAxis = (Range(yOrigin, yMax) by jump).toList

      xPathAxis = xAxis ::: (xAxis.last + jump) :: Nil
      yPathAxis = yAxis ::: (yAxis.last + jump) :: Nil
    }
  }


  override def paintComponent(g: Graphics2D) {
    updateGraphCanvasFields()

    g.setBackground(zorn.titaniumWhite)
    g.clearRect(0, 0, size.width, size.height)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    g.setStroke(stroke.eight)
    g.setColor(zorn.ivoryBlack)

    xAxis map (x =>
      yAxis map (y =>
        g.drawRect(x, y, jump, jump)))

    g.setStroke(stroke.roundSixteen)
    g.setColor(zorn.yellowOchreAlpha)

    currentPathCoords map { case (u, v) =>
      g.drawLine(xPathAxis(u._1), yPathAxis(u._2),
                 xPathAxis(v._1), yPathAxis(v._2))
    }
  }

  //def updateGraphDim(m: Int, n: Int): Unit = {
    //gridGraph = new GridGraph(m, n)


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
      //pathEdges = time (enumZDDValidPaths(zdd), "Path finding =>\t")
      //println("Algo2 Number of valid paths: "+ pathEdges.length +"\n")

    case 2 =>
      val ggV = gridGraph.graph.vertices
      val h = List(VertexPair(ggV(0), ggV.last))
      val zdd = time (algoTwo(gridGraph.graph, h), "Numberlink solver =>")
      pathEdges = time (enumZDDValidPaths2(zdd), "Path finding =>\t")
      println("Number of valid paths: "+ pathEdges.length +"\n")

  }
}

class DAGCanvas extends Panel {

  trait DAG {
    def x: Int
    def y: Int
    def width: Int
    def height: Int
  }

  //case class DAGEdge(x0: Int, y0: Int, x1: Int, y1: Int)

  case class DAGNode(x: Int, y: Int, width: Int, height: Int, lo: DAG, hi: DAG) extends DAG

  case class DAGLeaf(x: Int, y: Int, width: Int, height: Int) extends DAG


  val nodeWH = 64
  val jump = 128
  val fullJump = 256
  //val depth = g.edges.length + 1
  var startX = fullJump
  var startY = jump
  var currentDAG = List[DAGNode]()

  def drawDAGEdges(g: Graphics2D, node: DAG): Unit = {

    val half = nodeWH / 2
    val nodeCenter = (n: DAG) => (n.x + half, n.y + half)

    def helper(n: DAG): Unit = {
      n match {

        case DAGNode(x, y, w, h, lo: DAG, hi: DAG) =>
          val parent = nodeCenter(n)

          g.setStroke(stroke.fourDashed)
          val leftChild = nodeCenter(lo)
          g.drawLine(parent._1, parent._2, leftChild._1, leftChild._2)

          g.setStroke(stroke.four)
          val rightChild = nodeCenter(hi)
          g.drawLine(parent._1, parent._2, rightChild._1, rightChild._2)

          helper(lo)
          helper(hi)

        case _ =>
      }
    }

    helper(node)
  }

  def drawDAGNodes(g: Graphics2D, node: DAG): Unit = {
    node match {
      case DAGNode(x, y, w, h, lo: DAG, hi: DAG) =>
        g.fillOval(x, y, w, h)
        drawDAGNodes(g, lo)
        drawDAGNodes(g, hi)
      case DAGLeaf(x, y, w, h) =>
        g.fillOval(x, y, w, h)
    }
  }

  override def paintComponent(g: Graphics2D) {
    g.setBackground(zorn.titaniumWhite)

    g.clearRect(0, 0, size.width, size.height)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    g.setColor(zorn.ivoryBlack)

    g.setStroke(stroke.four)
    drawDAGEdges(g, currentDAG.head)

    g.setStroke(stroke.four)
    drawDAGNodes(g, currentDAG.head)

  }

  //def drawDAG(n: Node): Unit = {
  def repaintDAG(): Unit = {
    currentDAG = List(DAGNode(startX, startY, nodeWH, nodeWH, DAGLeaf(startX - jump, startY + jump, nodeWH, nodeWH), DAGLeaf(startX + jump, startY + jump, nodeWH, nodeWH)))
    repaint()
  }
}

object stroke {
  val four = new BasicStroke(4)
  val eight = new BasicStroke(8)
  val sixteen = new BasicStroke(16)

  val roundSixteen = new BasicStroke(16, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)

  val strokePattern = Array(8.0f)
  val fourDashed = new BasicStroke(4, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 1.0f, strokePattern, 0)
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
