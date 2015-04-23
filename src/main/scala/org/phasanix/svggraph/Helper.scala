package org.phasanix.svggraph

import java.awt.geom.{Rectangle2D, Point2D}
import RichColor.color2richcolor


/**
 * helper methods
 */
object Helper {

  private def pow10(x: Int): Int = {
    x match {
      case 1 => 10
      case 2 => 100
      case 3 => 1000
      case 4 => 10000
      case _ =>
        var count = x
        var p = 1
        while (count > 0) {
          p *= 10
          count -= 1
        }
        p
    }
  }

  /**
   * Fast append double value to string buffer with fixed dp
   * Thread-safe alternative to java.text.NumberFormat
   */
  def append(sb: StringBuilder, value: Double, dp: Int): Unit = {

    val intV = value.toInt
    sb.append(intV)

    if (dp > 0) {
      sb.append('.')
      val rem = Math.abs(value) - Math.abs(intV)
      val remInt = (pow10(dp) * rem).toInt
      sb.append(remInt)
    }
  }

  /** shorthand formatting methods for StringBuilder */
  implicit class StringBuilderOps(val sb: StringBuilder) extends AnyVal {

    def fmt(value: Double, dp: Int): StringBuilder = {
      append(sb, value, dp)
      sb
    }

    def fmt(value: Int): StringBuilder = {
      sb.append(value)
    }

    def fmt(value: String): StringBuilder = {
      sb.append(value)
    }

    def fmt(value: Char): StringBuilder = {
      sb.append(value)
    }

    def fmt(point: Point2D.Float): StringBuilder = {
      fmt(point.x.toDouble, 2)
      sb.append(',')
      fmt(point.y.toDouble, 2)
    }

    def fmt(point: (Int, Int)): StringBuilder = {
      sb.append(point._1)
        .append(',')
        .append(point._2)
    }

  }

  def fmt(value: Double, dp: Int): String = {
    val sb = new StringBuilder()
    sb.fmt(value, dp)
    sb.toString()
  }

  def fmt(value: Int): String = value.toString

  def fmt(value: Point2D.Float): String = {
    val sb = new StringBuilder()
    sb.fmt(value)
    sb.toString()
  }

  implicit class Rectangle2DOps(val rect: Rectangle2D.Float) extends AnyVal {
    def center: Point2D.Float = new Point2D.Float(rect.x + (rect.width / 2), rect.y + (rect.height / 2))
  }

  /** Draw a text element */
  def drawText(value: String, pos: Point2D.Float, attrs: Seq[Constants.AttrProvider] = Seq.empty)(implicit opts: Options): xml.Elem = {
    attrs.foldLeft(<text x={fmt(pos.x, 1)} y={fmt(pos.y, 1)}>{value}</text>) { (elt, attr) => attr(elt) }
  }

  /**
   * Draw grid on the chart area defined by opts, at the given positions
   */
  def drawGrid(vertical: Boolean, positions: Seq[Float])(implicit opts: Options) = {
    val area = opts.plotArea

    if (vertical) {
      val x1 = fmt(area.x, 1)
      val x2 = fmt(area.x + area.width, 1)
      positions.map { pos =>
          <line x1={x1} x2={x2} y1={fmt(pos, 1)} y2={fmt(pos, 1)} stroke={opts.draw.gridColor.asRgb}/>
      }
    } else {
      val y1 = fmt(area.y, 1)
      val y2 = fmt(area.height - area.y, 1)
      positions.map { pos =>
          <line x1={fmt(pos, 1)} x2={fmt(pos, 1)} y1={y1} y2={y2} stroke={opts.draw.gridColor.asRgb}/>
      }
    }
  }


}
