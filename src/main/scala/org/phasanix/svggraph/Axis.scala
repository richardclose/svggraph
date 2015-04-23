package org.phasanix.svggraph

import java.awt.geom.{Rectangle2D, Point2D}

abstract class Axis (
  val opts: Options,
  val title: String,
  val isVertical: Boolean,
  val isRight: Boolean)  {

  import Helper._
  import RichColor.color2richcolor
  import Constants._

  val origin: Point2D.Float = new Point2D.Float(opts.plotArea.x, opts.plotArea.y + opts.plotArea.height)
  val length: Float = if (isVertical) opts.plotArea.height else opts.plotArea.width
  val tickArea: Rectangle2D.Float =
    if (isVertical) {
      if (isRight) opts.layout.yTickAreaRight else opts.layout.yTickAreaLeft
    }
    else {
      opts.layout.xTickArea
    }

  val endpoint: Point2D.Float = if (isVertical)
    new Point2D.Float(origin.x, opts.plotArea.y)
  else
    new Point2D.Float(origin.x + length, origin.y)

  val minPos: Float = if (isVertical) origin.y else origin.x
  val maxPos: Float = if (isVertical) minPos - length else minPos + length

  /**
   * Draw baseline
   */
  def drawBaseline: xml.Elem =
      <line
      x1={fmt(origin.x, 1)}
      y1={fmt(origin.y, 1)}
      x2={fmt(endpoint.x, 1)}
      y2={fmt(endpoint.y, 1)}
      stroke-width={fmt(opts.strokeWidth, 1)}
      stroke={opts.draw.frameColor.asHex}/>

  /**
   * Draw axis title
   */
  def drawTitle: xml.Elem = {
    val pos = tickArea.center

    val xform = if (isVertical) "rotate(270,%3.0f,%3.0f)".format(pos.x, pos.y) else null

    val attrs = Seq(new AttrProvider("transform", xform), TextAnchor.Middle, BaselineAlignment.Middle)

    attrs.foldLeft(<text x={fmt(pos.x, 1)} y={fmt(pos.y, 1)}>{title}</text>) { (elt, attr) => attr(elt) }
  }

  /**
   * Generate evenly-spaced positions along the axis
   */
  def spaceEvenly(count: Int): Seq[Float] = {
    val spacing = (maxPos - minPos) / (count + 1)
    (0 until count).map { i =>
      minPos + (0.5f + i) * spacing
    }
  }

  /**
   * Draw a series of tick labels for the given axis
   */
  def drawLabels: Seq[xml.Elem]

  /**
   * Draw all decorations for this axis.
   */
  def decorate: Seq[xml.Elem]

}
