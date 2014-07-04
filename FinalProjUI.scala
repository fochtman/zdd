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
            val twoTwo = new RadioButton("2 x 2")
            val fourFour = new RadioButton("4 x 4")
            val eightEight = new RadioButton("8 x 8")
            val twoFour = new RadioButton("2 x 4")

            twoTwo.selected = true

            val mutex = new ButtonGroup(twoTwo, fourFour, eightEight, twoFour)
            contents ++= mutex.buttons
            listenTo(twoTwo, fourFour, eightEight, twoFour)

            reactions += {
              case ButtonClicked(`twoTwo`) =>
                visualization.canvas.updateGraphDim(2,2)
              case ButtonClicked(`fourFour`) =>
                visualization.canvas.updateGraphDim(4,4)
              case ButtonClicked(`eightEight`) =>
                visualization.canvas.updateGraphDim(8,8)
              case ButtonClicked(`twoFour`) =>
                visualization.canvas.updateGraphDim(2,4)
            }
          }

          contents += new BoxPanel(Orientation.Vertical) {
            border = CompoundBorder(TitledBorder(EtchedBorder, "Algorithm"), EmptyBorder(5,5,5,10))
            val one = new RadioButton("one")
            val two = new RadioButton("two")
            val mutex = new ButtonGroup(one, two)

            one.selected = true

            contents ++= mutex.buttons
            listenTo(one, two)

            reactions += {
              case ButtonClicked(`one`) =>
                visualization.slider.paintLabels = one.selected
              case ButtonClicked(`two`) =>
                visualization.slider.paintTicks = two.selected
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




    def collectOneEdges[T](n: Node[T]): List[Edge[T]] = n match {
      case Node(_, `oneTerminal`, _) => throw new NoSuchElementException
      case Node(_, `zeroTerminal`, `zeroTerminal`) => Nil
      case Node(p, `zeroTerminal`, `oneTerminal`) => p.edgeLabel :: Nil
      case Node(p, `zeroTerminal`, hi: Node[T]) =>
        n.params.edgeLabel :: Nil ::: collectOneEdges(hi)
      case n @ Node(p, lo: Node[T], hi: Node[T]) =>
        n.params.edgeLabel :: Nil ::: collectOneEdges(hi) ::: collectOneEdges(lo)
      case Node(p, lo: Node[T], _) =>
        collectOneEdges(lo)
    }

    /*
    //TODO: starting point of next workflow
    def processOneEdges[T](edges: List[Edge[T]], PairMatchings): Unit = edges match {
      //case Edge(e0, e1) if e0
    }
    */
}


class Canvas extends Panel {
  private val jump = 64

  // perhaps make this apart of a companion object to the canvas class
  private var gridGraph = new GridGraph(2, 2)

  // Hack Alert by way of the 640 constant.
  // center gridGraph
  private var xOrigin = (640 - (jump * gridGraph.colNum)) / 2
  private var yOrigin = (640 - (jump * gridGraph.rowNum)) / 2

  private var xMax = xOrigin + gridGraph.colNum * jump
  private var xAxis = Range(xOrigin, xMax) by jump

  private var yMax = yOrigin + gridGraph.rowNum * jump
  private var yAxis = Range(yOrigin , yMax) by jump

  val bzl = zorn.blended.length
  val twoStroke = new BasicStroke(2)
  val fourStroke = new BasicStroke(4)
  val eightStroke = new BasicStroke(8)
  val sixteenStroke = new BasicStroke(16)

  //g.setStroke(new BasicStroke(5, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL))
  override def paintComponent(g: Graphics2D) {
    g.setBackground(zorn.titaniumWhite)
    g.clearRect(0, 0, size.width, size.height)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)


    g.setStroke(eightStroke)
    g.setColor(zorn.ivoryBlack)
    xAxis map (i => yAxis map (j => g.drawRect(i, j, jump, jump)))

    g.setStroke(fourStroke)
    g.setColor(zorn.titaniumWhiteAlpha)
    xAxis map (i => yAxis map (j => g.drawRect(i, j, jump, jump)))

    g.setStroke(sixteenStroke)
    //g.setColor(zorn.cadmiumRedMediumAlpha)
    g.setColor(new Color(0, 0, 250, 32))
    xAxis map (i => yAxis map (j => g.drawLine(i, j+jump, i+jump, j+jump)))
    //g.setColor(zorn.blended(Random.nextInt(bzl)))
    //g.fillRect(i, j, jump, jump)
  }

  def updateGraphDim(m: Int, n: Int) {
    gridGraph = new GridGraph(m, n)

    xOrigin = (640 - (jump * gridGraph.colNum)) / 2
    yOrigin = (640 - (jump * gridGraph.rowNum)) / 2

    xMax = xOrigin + gridGraph.colNum * jump
    xAxis = Range(xOrigin, xMax) by jump

    yMax = yOrigin + gridGraph.rowNum * jump
    yAxis = Range(yOrigin , yMax) by jump
  }

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
  val titaniumWhiteAlpha = new Color(244, 237, 237, 64)
  val cadmiumRedMediumAlpha = new Color(196, 1, 45, 32)
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
