package org.phasanix.svggraph

import java.awt.geom.Point2D

/**
 */
class Chart {

  import Helper._

  def svgPath(points: Seq[(Int, Int)]): String = {

    val sb = new StringBuilder()

    if (points.length > 1) {
      sb.fmt('M').fmt(' ')
        .fmt(points.head)
        .append('L')

      points.tail.foreach(p => sb.append(' ').fmt(p))
    }

    sb.toString()
  }


  def svgPathPts(points: Seq[Point2D.Float]): String = {

    val sb = new StringBuilder()

    if (points.length > 1) {
      sb.fmt('M').fmt(' ')
        .fmt(points.head)
        .append('L')

      points.tail.foreach(p => sb.append(' ').fmt(p))
    }

    sb.toString()
  }

}
