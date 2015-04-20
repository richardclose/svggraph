package org.phasanix.svggraph

import java.awt.geom.{Rectangle2D, Point2D}

/**
 * helper methods
 */
object Helper {

  /**
   * Fast append double value to string buffer with fixed dp
   * Thread-safe alternative to java.text.NumberFormat
   */
  def append(sb: StringBuilder, value: Double, dp: Int): Unit = {
    val insertAt = sb.length

    val absV = if (value < 0) {
      sb.append('-')
      -value
    } else value

    var intV = absV.toInt

    while (intV > 0) {
      sb.insert(insertAt, ((intV % 10) + '0').toChar)
      intV /= 10
    }

    if (dp > 0) {
      val pow = dp match {
        case 1 => 10
        case 2 => 100
        case 3 => 1000
        case 4 => 10000
        case _ =>
          var count = dp
          var p = 1
          while (count > 0) {
            p *= 10
            count -= 1
          }
          p
      }

      var decV = ((absV % 1) * pow).toInt
      sb.append('.')
      val insertDp = sb.length
      var count = dp
      while (count > 0) {
        sb.insert(insertDp, ((decV % 10) + '0').toChar)
        decV /= 10
        count -= 1
      }
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
      sb
    }

    def fmt(value: String): StringBuilder = {
      sb.append(value)
      sb
    }

    def fmt(value: Char): StringBuilder = {
      sb.append(value)
      sb
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
      sb
    }

  }

  implicit class Rectangle2DOps(val rect: Rectangle2D.Float) extends AnyVal {
    def center: Point2D.Float = new Point2D.Float(rect.x + (rect.width / 2), rect.y + (rect.height / 2))
  }

}
