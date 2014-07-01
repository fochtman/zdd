/**
 * Created by Tyler on 6/26/2014.
 */

import scala.swing._
import scala.swing.BorderPanel.Position._
import event._
import java.awt.{ Color, Graphics2D, RenderingHints, BasicStroke }
import scala.util.Random
import scala.math.{ sin, cos, round, Pi }

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
    val pG = pqrsGraph.g
    val jZDD = algorithmOne(pG)
    println(countZDDOnePaths(jZDD))
    */
    /*
    val fourG = gridGraph4by4.g
    val ZDD = algorithmTwo(fourG, gridGraph4by4.h)
    println(countZDDOnePaths(ZDD))
    */

    val gGraph = gridGraph.g
    val jj = algorithmTwo(gGraph, gridGraph.h)
    println(countZDDOnePaths(jj))

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
}

object gridGraph4by4 {
  val vList = List.range(1, 5) map (x => Vertex(x, x - 1))
  val e1 = Edge(vList(0), vList(1))
  val e2 = Edge(vList(0), vList(2))
  val e3 = Edge(vList(1), vList(3))
  val e4 = Edge(vList(2), vList(3))
  val eList = List(e1, e2, e3, e4)
  val g = new Graph(vList, eList)

  val h = List(VertexPair(vList(0), vList(3)))
}

object gridGraph {

  val vList = List.range(1,13) map (x => Vertex(x, x-1))
  val e1 = Edge(vList(0), vList(1))
  val e2 = Edge(vList(0), vList(3))
  val e3 = Edge(vList(1), vList(2))
  val e4 = Edge(vList(1), vList(4))
  val e5 = Edge(vList(2), vList(5))
  val e6 = Edge(vList(3), vList(4))
  val e7 = Edge(vList(3), vList(6))
  val e8 = Edge(vList(4), vList(5))
  val e9 = Edge(vList(4), vList(7))
  val e10 = Edge(vList(5), vList(8))
  val e11 = Edge(vList(6), vList(7))
  val e12 = Edge(vList(7), vList(8))
  val eList = List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12)
  val g = new Graph(vList, eList)

  val h = List(VertexPair(vList(0), vList(8)))
  //val pp = algorithmOne(gg)
}

class Canvas extends Panel {
  val yellowOchre = new Color(245, 197, 44)
  val cadmiumRedMedium = new Color(196, 1, 45)
  val ivoryBlack = new Color(40, 36, 34)
  val titaniumWhite = new Color(244, 237, 237)

  val zornBlend = new Color(181, 117, 90, 50)
  val zornYBW = List[Color](yellowOchre, ivoryBlack, titaniumWhite)
  val zornPalette = List[Color](yellowOchre, cadmiumRedMedium, ivoryBlack, titaniumWhite)
  val blendedZorn = blendPalette(zornPalette)
  //val blendedZorn = blendPalette(zornYBW)
  val bzl = blendedZorn.length
  val fiveStroke = new BasicStroke(5)
  val twoStroke = new BasicStroke(2)
  val jump = 64
  val innerRect = jump + jump/2
  val innerRectWidthHeight = size.width - (innerRect * 2)

  //g.setStroke(new BasicStroke(5, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL))
  override def paintComponent(g: Graphics2D) {
    g.clearRect(0, 0, size.width, size.height)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setBackground(ivoryBlack)

    //g.setColor(zornBlend)
    //g.fillRect(innerRect, innerRect, innerRectWidthHeight, innerRectWidthHeight)
    for (i <- Range(jump, size.width-innerRect) by jump;
         j <- Range(jump, size.height-innerRect) by jump) {

      g.setColor(zornBlend)
      g.setStroke(fiveStroke)
      g.drawRect(i, j, jump, jump)

      g.setColor(ivoryBlack)
      g.setStroke(twoStroke)
      g.drawRect(i, j, jump, jump)

      //g.setColor(blendedZorn(Random.nextInt(bzl)))
      //g.fillOval(i, j, jump, jump)
    }
  }

  def blendPalette(palette: List[Color]): List[Color] = {
    def f(c1: Int, c2: Int) = (c1+c2)/2
    for (i <- palette; j <- palette) yield {
      new Color(f(i.getRed(), j.getRed()), f(i.getGreen(), j.getGreen()), f(i.getBlue(), j.getBlue()))
    }
  }
}
