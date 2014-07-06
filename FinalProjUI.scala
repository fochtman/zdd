import java.awt.{ Color, Graphics2D, RenderingHints, BasicStroke }
import swing._
import swing.event._
import Swing._
import BorderPanel.Position._

import Graph._
import ZDD._

object FinalProjUI  extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "fun with zdds"

    contents = new BorderPanel {

      val tabs = new TabbedPane {
        import TabbedPane._

        val parameters = new FlowPanel {
          border = Swing.EmptyBorder(5,5,5,5)
          contents += new BoxPanel(Orientation.Vertical) {
            border = CompoundBorder(TitledBorder(EtchedBorder, "Grid Size"), EmptyBorder(5,5,5,10))
            val oneOne = new RadioButton("1 x 1")
            val twoTwo = new RadioButton("2 x 2")
            val threeThree = new RadioButton("3 x 3")
            val fourFour = new RadioButton("4 x 4")
            val eightEight = new RadioButton("8 x 8")
            val twoFour = new RadioButton("2 x 4")

            val mutex = new ButtonGroup(oneOne, twoTwo, threeThree, fourFour, eightEight, twoFour)
            contents ++= mutex.buttons
            listenTo(oneOne, twoTwo, fourFour, threeThree, eightEight, twoFour)

            reactions += {
              case ButtonClicked(`oneOne`) =>
                visualization.canvas.updateGraphDim(2,2)
              case ButtonClicked(`twoTwo`) =>
                visualization.canvas.updateGraphDim(3,3)
              case ButtonClicked(`threeThree`) =>
                visualization.canvas.updateGraphDim(4,4)
              case ButtonClicked(`fourFour`) =>
                visualization.canvas.updateGraphDim(5,5)
              case ButtonClicked(`eightEight`) =>
                visualization.canvas.updateGraphDim(9,9)
              case ButtonClicked(`twoFour`) =>
                visualization.canvas.updateGraphDim(3,5)
            }
          }

          contents += new BoxPanel(Orientation.Vertical) {
            border = CompoundBorder(TitledBorder(EtchedBorder, "Algorithm"), EmptyBorder(5,5,5,10))
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
            //value = tabs.selection.index
            value = 0
            max = 100
            majorTickSpacing = 1
          }

          slider.paintTicks = true
          layout(canvas) = Center
          layout(slider) = South
          /*
        listenTo(slider)
        reactions += {
          case ValueChanged(`slider`) =>
            if(!slider.adjusting || reactLive) tabs.selection.index = slider.value
            }
            */
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

  private var pathEdges = List[List[Edge[Int]]]()

  private val jump = 64

  // perhaps make this apart of a companion object to the canvas class
  private var gridGraph = new GridGraph(2, 2)

  // Hack Alert by way of the 640 constant.
  // center gridGraph
  private var xOrigin = (640 - (jump * gridGraph.colNum-1)) / 2
  private var yOrigin = (640 - (jump * gridGraph.rowNum-1)) / 2

  private var xMax = xOrigin + (gridGraph.colNum-1) * jump
  private var xAxis = Range(xOrigin, xMax) by jump

  private var yMax = yOrigin + (gridGraph.rowNum-1) * jump
  private var yAxis = Range(yOrigin , yMax) by jump

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
    println(xAxis)
    println(yAxis)
    xAxis map (i => yAxis map (j => g.drawRect(i, j, jump, jump)))
    //g.drawPolygon(Array(0, 60, 60, 0), Array(0, 0, 60, 60), 4)

    //g.setStroke(new BasicStroke(16, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
    //g.setColor(zorn.yellowOchreAlpha)
    //xAxis map (i => yAxis map (j => g.drawLine(i, j+jump, i+jump, j+jump)))
  }
  //g.setColor(zorn.cadmiumRedMediumAlpha)
  //g.setColor(zorn.blended(Random.nextInt(bzl)))
  //g.setStroke(fourStroke)
  //g.setColor(zorn.titaniumWhiteAlpha)
  //xAxis map (i => yAxis map (j => g.drawRect(i, j, jump, jump)))

  def updateGraphDim(m: Int, n: Int) {
    gridGraph = new GridGraph(m, n)

    xOrigin = (size.width - (jump * (gridGraph.colNum-1))) / 2
    yOrigin = (size.height - (jump * (gridGraph.rowNum-1))) / 2

    xMax = xOrigin + (gridGraph.colNum-1) * jump
    xAxis = Range(xOrigin, xMax) by jump

    yMax = yOrigin + (gridGraph.rowNum-1) * jump
    yAxis = Range(yOrigin , yMax) by jump
  }

  // in general need a method that builds zdd based on chosen grid graph and algorithm algorithm
  // ---> for now if it is algorithm 2 i will force them to choose top left and bottom right vertices
  // once the zdd is constructed, find all one paths,
  // ---> a) store them in a list,
  //      b) link the slider to index the list
  //      c) update the slider ticks
  //      d) repaint when slider is moved

  // each tick will be a List(List[Edge[T]))

  def collectOneEdges[T](n: Node[T]): List[Edge[T]] = n match {
    case Node(_, `oneTerminal`, _) => throw new NoSuchElementException
    case Node(_, `zeroTerminal`, `zeroTerminal`) => Nil
    case Node(p, `zeroTerminal`, `oneTerminal`) => p.edgeLabel :: Nil
    case nn @ Node(p, `zeroTerminal`, hi: Node[T]) =>
      nn.params.edgeLabel :: Nil ::: collectOneEdges(hi)
    case nn @ Node(p, lo: Node[T], hi: Node[T]) =>
      nn.params.edgeLabel :: Nil ::: collectOneEdges(hi) ::: collectOneEdges(lo)
    case Node(p, lo: Node[T], _) =>
      collectOneEdges(lo)
  }

  def collectPathEdges(choice: Int) = {
    val zdd = if (choice == 1)
        algorithmOne(gridGraph.graph)
      else {
        val h = List(VertexPair(gridGraph.graph.vertices(0), gridGraph.graph.vertices.last))
        algorithmTwo(gridGraph.graph, h)
    }
    val numOnePaths = countZDDOnePaths(zdd)
    //slider.max = numOnePaths
    println(numOnePaths)
    val unoEdges = collectOneEdges(zdd)
    println("--------------------------")
    unoEdges.foreach(println)//(unoEdges)
    println("--------------------------")
    // val drawableEdges = processOneEdges(oneEdges)


  }
  /*
  //TODO: starting point of next workflow
  def processOneEdges[T](edges: List[Edge[T]], PairMatchings): Unit = edges match {
    //case Edge(e0, e1) if e0
  }
  */

}

object pqrsGraph {
  val p = Vertex('p', 0)
  val q = Vertex('q', 1)
  val r = Vertex('r', 2)
  val s = Vertex('s', 3)
  val vertexList = List(p, q, r, s)
  val pq = Edge(p, q)
  val pr = Edge(p, r)
  val qr = Edge(q, r)
  val qs = Edge(q, s)
  val edgeList = List(pq, pr, qr, qs)
  val g = new Graph(vertexList, edgeList)

  val h = List(VertexPair(p, s))
  /*
  val pG = pqrsGraph.g
  val jZDD = algorithmTwo(pG, pqrsGraph.h)
  println("pg: "+ countZDDOnePaths(jZDD))
  */
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
