package org.phasanix.svggraph

import java.awt.geom.{Rectangle2D, Point2D}
import java.util.{Calendar => Cal, Date, GregorianCalendar}
import Helper._
import RichColor.color2richcolor
import Constants._

abstract class Axis (
  val opts: Options,
  val title: String,
  val isVertical: Boolean,
  val isRight: Boolean)  {

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

object Axis {

  /**
   * Axis for which datapoints are positioned according to their value (e.g. x/y scatterplot)
   */
  class ScaleAxis (
    opts: Options,
    title: String,
    isVertical: Boolean,
    isRight: Boolean,              
    val minValue: Double,
    val maxValue: Double,
    val isDate: Boolean,
    val isIntegral: Boolean,
    val tickFmt: Double => String) extends Axis(opts, title, isVertical, isRight) {

    implicit val implicitOpts = opts

    private[this] val factor = (maxPos - minPos) / (maxValue - minValue)

    /**
     * Translate the given value into a position on this axis.
     */
    def mapValue(value: Double): Float = {
      if (value < minValue)
        minPos
      else if (value > maxValue)
        maxPos
      else
        minPos + ((value - minValue) * factor).toFloat
    }

    /**
     * Translate the given value to a point on the axis
     */
    def mapValueToPosition(value: Double): Point2D.Float = {
      val pos = mapValue(value)
      val ret = new Point2D.Float()
      if (isVertical)
        ret.setLocation(origin.x, pos)
      else
        ret.setLocation(pos, origin.y)
      ret
    }

    /**
     * Draw a series of tick labels for the given axis
     */
    def drawLabels(vals: Seq[Double]): Seq[xml.Elem] = {
      for (v <- vals) yield {
        val p = this.mapValueToPosition(v)
        val text = tickFmt(v)
        val anchor = if (isVertical) TextAnchor.End else TextAnchor.Middle

        // Tweak exact position
        if (isVertical) {
          p.x -= 2
        } else {
          p.y += opts.draw.lineSpacing
        }

        drawText(text, p, Seq(anchor))
      }
    }

    def decorate: Seq[xml.Elem] = {
      val values = Tick.values(opts, this)
      val positions = values.map(this.mapValue)
      drawLabels(values) ++
        drawGrid(isVertical, positions) :+
        drawTitle
    }

    /**
     * Draw a series of tick labels for the given axis
     */
    def drawLabels: Seq[xml.Elem] = drawLabels(Tick.values(opts, this))

    /**
     * Draw tick marks and labels
     */
    def drawTickmarksAndLabels: Seq[xml.Elem] = {
      val values = Tick.values(opts, this)
      val labels = drawLabels(values)
      val marks = for (v <- values) yield {
        val ticklen = 4
        val pos = mapValueToPosition(v)
        val pos1 = new Point2D.Float(pos.x, pos.y)
        if (isVertical) pos1.x -= ticklen else pos1.y += ticklen
          <line x1={fmt(pos.x, 1)} y1={fmt(pos.y, 1)} x2={fmt(pos1.x, 1)} y2={fmt(pos1.y, 1)} stroke={RichColor.Gray.asRgb} stroke-width="1.0" />
      }
      marks ++ labels
    }

    /** Draw labels, tick marks and baseline */
    def drawAll: Seq[xml.Elem] = drawTickmarksAndLabels :+ drawBaseline

    /**
     * Tick interval positions
     */
    def tickIntervals(rawCount: Int): Iterator[Double] =
      if (isDate) dateTickIntervals(rawCount) else numericTickIntervals(rawCount, isIntegral)

    /**
     * Generate appropriately sized tick intervals
     */
    private def numericTickIntervals(rawCount: Int, integral: Boolean) = new Iterator[Double] {
      import java.lang.Math.{ pow, log10 }

      // Interval, such that about rawCount intervals will fit into 
      // the range
      private[this] val interval = {
        val rawSize = (maxValue - minValue) / rawCount
        val scale = pow(10, log10(rawSize).toInt - 1)
        val s2 = rawSize / scale
        val m = scaleBreaks
          .find(e => s2 > e._1 && s2 < e._2)
          .fold(50.0)(_._1)
        // println("rawSize=" + rawSize + " scale=" + scale + " s2=" + s2 + " m=" + m)
        val x = m * scale
        if (integral && x < 1.0) 1.0 else x
      }

      private[this] var nextValue = {
        ((minValue / interval).toInt + 1) * interval
      }

      def hasNext = nextValue < maxValue

      def next() = {
        val ret = nextValue
        nextValue += interval
        ret
      }
    }

    /**
     * Tick interval generator for date values (i.e. Date.getTime() -> Long: Double)
     */
    def dateTickIntervals(rawCount: Int) = new Iterator[Double] {

      private[this] val interval = {
        val rawInterval = (maxValue - minValue) / rawCount
        val int1 = dateScaleBreaks
          .find(e => rawInterval > e._2._1 && rawInterval < e._2._2)
          .map(_._1).getOrElse(Cal.YEAR, 1)

        if (int1._1 == Cal.YEAR) {
          val x = (rawInterval / 365).toInt
          (Cal.YEAR, if (x == 0) 1 else x)
        } else {
          int1
        }
      }
      private[this] val cal = new GregorianCalendar()
      private var _passCount = 0

      private[this] var nextValue: Double = {
        cal.setTime(new Date(minValue.toLong))
        interval._1 match {
          case Cal.YEAR =>
            cal.set(Cal.MONTH, 1)
            cal.set(Cal.DAY_OF_MONTH, 1)
          case Cal.MONTH =>
            cal.set(Cal.DAY_OF_MONTH, 1)
          case Cal.WEEK_OF_YEAR =>
            cal.set(Cal.DAY_OF_WEEK, 1)
          case _ =>
        }

        while (cal.getTimeInMillis < minValue.toLong)
          cal.add(interval._1, interval._2)

        cal.getTimeInMillis
      }

      def hasNext = nextValue < maxValue && _passCount < 100

      def next() = {
        _passCount += 1
        val ret = nextValue
        cal.add(interval._1, interval._2)

        nextValue = cal.getTimeInMillis
        ret
      }
    }

    override def toString =
      "<Axis factor=" + factor + " minValue=" + minValue + " maxValue=" +
        maxValue + " minPos=" + minPos + " maxPos=" + maxPos + " />"
  }

  /**
   * Multipliers to get integral tick intervals for a numeric range
   */
  val scaleBreaks = Seq(0.0, 1.0, 2.0, 2.5, 5.0, 10.0, 20.0, 25.0, 50.0)
    .sliding(2)
    .map(e => (e(0), e(1)))
    .toSeq

  /**
   * Date interval boundaries to get reasonable tick intervals for a date range
   */
  val dateScaleBreaks = {

    val xs = Seq (
      Cal.MINUTE -> 1,
      Cal.MINUTE -> 60,
      Cal.HOUR -> 2,
      Cal.DAY_OF_MONTH -> 1,
      Cal.WEEK_OF_YEAR -> 1,
      Cal.WEEK_OF_YEAR -> 2,
      Cal.MONTH -> 1,
      Cal.MONTH -> 2,
      Cal.MONTH -> 3,
      Cal.MONTH -> 6,
      Cal.YEAR -> 1).map { e =>
      val c = new GregorianCalendar()
      c.setTimeInMillis(0L)
      c.add(e._1, e._2)
      (e, c.getTimeInMillis.toDouble)
    }

    xs.sliding(2)
      .map { e => e(1)._1 ->(e(0)._2, e(1)._2) }
      .toSeq
  }



}
