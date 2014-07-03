import scala.swing._
import scala.swing.BorderPanel.Position._
import java.awt.{ Color, Graphics2D, RenderingHints, BasicStroke }
//import scala.util.Random
//import event._
//import scala.math.{ sin, cos, round, Pi }

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
    val big = Graph.gridGraph(2,2)
    val h = List(VertexPair(big.vertices(0), big.vertices.last))
    val bigZDD = algorithmTwo(big, h)
    println("finished building")
    println("big: "+ countZDDOnePaths(bigZDD))
  }
}

object zorn  {
  val yellowOchre = new Color(245, 197, 44)
  val cadmiumRedMedium = new Color(196, 1, 45)
  val ivoryBlack = new Color(40, 36, 34)
  val titaniumWhite = new Color(244, 237, 237)
  val blend = new Color(181, 117, 90, 50)
  val YBW = List[Color](yellowOchre, ivoryBlack, titaniumWhite)
  val palette = List[Color](yellowOchre, cadmiumRedMedium, ivoryBlack, titaniumWhite)
  val blended = blendPalette(palette)

  def blendPalette(palette: List[Color]): List[Color] = {
    def f(c1: Int, c2: Int) = (c1+c2)/2
    for (i <- palette; j <- palette) yield {
      new Color(f(i.getRed, j.getRed), f(i.getGreen, j.getGreen), f(i.getBlue, j.getBlue))
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
    g.clearRect(0, 0, size.width, size.height)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setBackground(zorn.ivoryBlack)

    //g.setColor(zornBlend)
    //g.fillRect(innerRect, innerRect, innerRectWidthHeight, innerRectWidthHeight)
    for (i <- Range(jump, size.width-innerRect) by jump;
         j <- Range(jump, size.height-innerRect) by jump) {

      g.setColor(zorn.ivoryBlack)
      g.setStroke(twoStroke)
      g.drawRect(i, j, jump, jump)

      //g.setColor(zorn.ivoryBlack)
      //g.setStroke(twoStroke)
      //g.drawRect(i, j, jump, jump)

      //g.setColor(blendedZorn(Random.nextInt(bzl)))
      //g.fillOval(i, j, jump, jump)
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
