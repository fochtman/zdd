/**
 * Created by Tyler on 6/26/2014.
 */

import scala.swing._
import scala.swing.BorderPanel.Position._
import event._
import java.awt.{ Color, Graphics2D, RenderingHints, BasicStroke }
import scala.util.Random
import scala.math.{ sin, cos, round, Pi }

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
  }
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
