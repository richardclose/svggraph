package org.phasanix.svggraph

import java.awt.Color
import java.awt.geom.Point2D
import RichColor.color2richcolor

/**
 * Shape of point on graph
 */
sealed abstract class PointStyle(val name: String) {

  def render(color: Color, radius: Float, x: Int, y: Int, fillColor: Option[Color]): xml.Elem

  def render(color: Color, radius: Float, x: Int, y: Int): xml.Elem = render(color, radius, x, y, None)

  def render(color: Color, radius: Float, x: Int, y: Int, fillColor: Color): xml.Elem = render(color, radius, x, y, Some(fillColor))

  def render(color: Color, radius: Float, pos: Point2D.Float): xml.Elem = render(color, radius, pos.x.toInt, pos.y.toInt, None)

  def render(color: Color, radius: Float, pos: Point2D.Float, fillColor: Color): xml.Elem = render(color, radius, pos.x.toInt, pos.y.toInt, Some(fillColor))

  def render(color: Color, radius: Float, pos: Point2D.Float, fillColor: Option[Color]): xml.Elem = render(color, radius, pos.x.toInt, pos.y.toInt, fillColor)

  protected def _c(color: Option[Color]) =
    color.map(_.asHex).getOrElse("none")
}

object PointStyle {

  import Helper._

  object NoPoint extends PointStyle("none") {
    def render(color: Color, radius: Float, x: Int, y: Int, fillColor: Option[Color]): xml.Elem =
        <g/>
  }

  object Circle extends PointStyle("circle") {
    def render(color: Color, radius: Float, x: Int, y: Int, fillColor: Option[Color]): xml.Elem =
        <circle fill={_c(fillColor)} stroke={color.asRgb} r={fmt(radius, 1)} cx={fmt(x)} cy={fmt(y)}/>
  }

  object Square extends PointStyle("square") {
    def render(color: Color, radius: Float, x: Int, y: Int, fillColor: Option[Color]): xml.Elem = {
      val x1 = fmt(x - radius / 2, 1)
      val y1 = fmt(y - radius / 2, 1)
      val h = fmt(radius, 1)
        <rect fill={_c(fillColor)} stroke={color.asRgb} x={x1} y={y1} height={h} width={h}/>
    }
  }

  object Triangle extends PointStyle("triangle") {
    def render(color: Color, radius: Float, x: Int, y: Int, fillColor: Option[Color]): xml.Elem = {
      val rx = radius * 0.886 // sin(30)
      val ry = radius * 0.5 //cos(30)
      val start = (x, (y - radius).toInt)
      val points = Seq(start, ((x - rx).toInt, (y + ry).toInt), ((x + rx).toInt, (y + ry).toInt), start)
        <path fill={_c(fillColor)} stroke={color.asRgb} d={Chart.svgPath(points)}/>
    }
  }

  // Tall downward pointed triangle, where the point is at the given coordinates.
  object PointyTriangle extends PointStyle("pointy") {
    def render(color: Color, radius: Float, x: Int, y: Int, fillColor: Option[Color]): xml.Elem = {
      val pb = new PathBuilder(new Point2D.Float(x, y))
      pb.moveRel(-radius / 2, -2 * radius)
        .moveRel(radius, 0f)
        .toStart

        <path fill={_c(fillColor)} stroke={color.asRgb} d={pb.path}/>
    }
  }

  object VerticalLine extends PointStyle("vline") {
    def render(color: Color, radius: Float, x: Int, y: Int, fillColor: Option[Color]): xml.Elem =
        <line x1={fmt(x)} x2={fmt(x)} y1={fmt(y - radius, 1)} y2={fmt(y + radius, 1)} stroke={color.asRgb}/>
  }

  object Diamond extends PointStyle("diamond") {
    def render(color: Color, radius: Float, x: Int, y: Int, fillColor: Option[Color]): xml.Elem = {
      val start = new Point2D.Float(x - radius, y)
      val pb = new PathBuilder(start)
      pb.moveRel(radius, -radius)
        .moveRel(radius, radius)
        .moveRel(-radius, radius)
        .moveAbs(start)
        <path fill={_c(fillColor)} stroke={color.asRgb} d={pb.path}/>
    }
  }

  object Cross extends PointStyle("cross") {
    def render(color: Color, radius: Float, x: Int, y: Int, fillColor: Option[Color]): xml.Elem = {
      val r = radius / 2
      val sw = fmt(r, 1)
      val (x1, x2, y1, y2) = (fmt(x - r, 1), fmt(x + r, 1), fmt(y - r, 1), fmt(y + r, 1))

      <g>
        <line x1={x1} y1={y1} x2={x2} y2={y2} stroke={color.asRgb} stroke-width={sw}/>
        <line x1={x1} y1={y2} x2={x2} y2={y1} stroke={color.asRgb} stroke-width={sw}/>
      </g>
    }
  }

  /** Look up style with the given name */
  def get(name: String) = styleMap.getOrElse(name, None)

  private val styleMap = Seq(Circle, Square, Triangle, Cross, Diamond, VerticalLine, PointyTriangle).map(ps => (ps.name, ps)).toMap
}
