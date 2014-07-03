import scala.swing._
import scala.swing.BorderPanel.Position._
import java.awt.{ Color, Graphics2D, RenderingHints, BasicStroke }
import scala.util.Random

import Graph._
import ZDD._

object FinalProjUI  extends SimpleSwingApplication {
  def top = new MainFrame {
    val canvas = new Canvas {
      preferredSize = new Dimension(1024, 512)
    }

    contents = new BorderPanel {
      layout(canvas) = Center
    }

    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(Action("Exit") {
          sys.exit(0)
        })
      }
    }

    /*
    val fourGb = Graph.gridGraph(2, 2)
    val threeGb = Graph.gridGraph(3, 3)
    //val h = List(VertexPair(vList(0), vList(3)))
    val ZDDb = algorithmTwo(fourGb, gridGraph2by2.h)
    println("fourGb: "+ countZDDOnePaths(ZDDb))
    val g88 = algorithmTwo(threeGb, gridGraph.h)
    println("eightG: "+ countZDDOnePaths(g88))
    */
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

    val big = Graph.gridGraph(3, 3)
    //val h = List(VertexPair(big.vertices(0), big.vertices.last), VertexPair(big.vertices(1), big.vertices(5)))
    val h = List(VertexPair(big.vertices(0), big.vertices(6)), VertexPair(big.vertices(1), big.vertices(5)))
    h.foreach(println)
    val bigZDD = algorithmTwo(big, h)
    val oneEdges = collectOneEdges(bigZDD)
    oneEdges.foreach(println)
  }
}

object zorn  {
  val yellowOchre = new Color(245, 197, 44)
  val cadmiumRedMedium = new Color(196, 1, 45)
  val ivoryBlack = new Color(40, 36, 34)
  val titaniumWhite = new Color(244, 237, 237)
  val titaniumWhiteAlpha = new Color(244, 237, 237, 75)
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

class Canvas extends Panel {

  val bzl = zorn.blended.length
  val twoStroke = new BasicStroke(2)
  val fourStroke = new BasicStroke(4)
  val fiveStroke = new BasicStroke(5)
  val jump = 64
  val innerRect = jump + jump/2
  val innerRectWidthHeight = size.width - (innerRect * 2)

  //g.setStroke(new BasicStroke(5, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL))
  override def paintComponent(g: Graphics2D) {
    g.setBackground(zorn.titaniumWhite)
    g.clearRect(0, 0, size.width, size.height)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    for (i <- Range(jump, size.width-innerRect) by jump;
         j <- Range(jump, size.height-innerRect) by jump) {

      //g.setColor(zorn.ivoryBlack)
      //g.setStroke(twoStroke)
      //g.drawRect(i, j, jump, jump)

      g.setStroke(new BasicStroke(16))//, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL))
      g.setColor(zorn.ivoryBlack)
      g.drawRect(i, j, jump, jump)

      g.setStroke(fiveStroke)
      g.setColor(zorn.titaniumWhiteAlpha)
      //g.setColor(zorn.cadmiumRedMedium)
      g.drawLine(i, j, i, j+jump)
      g.drawLine(i, j+jump, i+jump, j+jump)

      //g.setColor(zorn.blended(Random.nextInt(bzl)))
      //g.fillRect(i, j, jump, jump)
    }
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
