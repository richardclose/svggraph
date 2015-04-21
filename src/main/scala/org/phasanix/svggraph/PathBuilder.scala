package org.phasanix.svggraph

import java.awt.geom.Point2D


/**
 * Convenience interface for constructing SVG path strings,
 * absolute positions only
 * TODO: split into absolute and relative path implementations
 */
class PathBuilder(start: Point2D.Float) {

  import Helper._

  private val points = collection.mutable.ArrayBuffer.empty[Either[Point2D.Float, String]]
  private var current = start

  points += Left(start)

  /** Move to absolute position */
  def moveAbs(point: Point2D.Float): PathBuilder = {
    current = point
    points += Left(point)
    this
  }

  /** Insert path op string before current point */
  def op(s: String): PathBuilder = {
    points += Right(s)
    this
  }

  /** Move relative to current position */
  def moveRel(dx: Float, dy: Float): PathBuilder = {
    val p = new Point2D.Float(current.x, current.y)
    p.x += dx
    p.y += dy
    moveAbs(p)
    this
  }

  /**Return to start */
  def toStart: PathBuilder = {
    moveAbs(start)
    this
  }

  /** path string */
  def path: String = {
    val sb = new StringBuilder
    sb.fmt('M')
    for (x <- points) {
      sb.fmt(' ')
      x match {
        case Left(pt) => sb.fmt(pt)
        case Right(s) => sb.fmt(s)
      }
    }
    sb.toString()
  }

}
