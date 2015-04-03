package org.phasanix.svggraph

import java.awt.geom.{Point2D, Rectangle2D}

/**
 * Configuration options.
 *
 * Measurements are all in px.
 */
case class Options (
  layout: Options.Layout,
  font: Options.Font
)

object Options {

  /** Basic set of options */
  def basic: Options = Options (
    Layout.basic(chartWidth = 500, chartHeight = 300),
    Font.basic()
  )

  /**
   * Chart layout. Chart regions are expressed as Rectangle2D, with measurements
   * in px, and the origin of the coordinate system is at the bottom left of the
   * chart area.
   *
   * @param chartWidth width of graph, in px.
   * @param chartHeight height of graph, in px.
   * @param xTickMargin height of x axis margin.
   * @param yTickMarginLeft width of left y axis margin.
   * @param yTickMarginRight width of right y axis margin (if any).
   * @param plotMargin reserved space around plot area to accommodate
   *                   points that fall on the x or y axis.
   */
  case class Layout (
    chartWidth: Int,
    chartHeight: Int,
    xTickMargin: Int,
    yTickMarginLeft: Int,
    yTickMarginRight: Int,
    plotMargin: Int
  ) {

    /** Area of whole chart */
    def chartArea: Rectangle2D.Float = new Rectangle2D.Float(0f, 0f, chartWidth, chartHeight)

    /** Area where chart contents are plotted */
    def plotArea: Rectangle2D.Float  = {
      val x = yTickMarginLeft + plotMargin
      val y = xTickMargin + plotMargin
      new Rectangle2D.Float(x, y, chartWidth - x - plotMargin - yTickMarginRight, chartHeight - y)
    }

    /** Area of x axis title and tick marks */
    def xTickArea: Rectangle2D.Float =
      new Rectangle2D.Float(yTickMarginLeft + plotMargin, 0, chartWidth - yTickMarginLeft - yTickMarginRight - (2*plotMargin), xTickMargin)

    /** Area of left y axis title and tick marks */
    def yTickAreaLeft: Rectangle2D.Float =
      new Rectangle2D.Float(0, xTickMargin + plotMargin, yTickMarginLeft, chartHeight - xTickMargin - plotMargin)

    /** Area of left y axis title and tick marks */
    def yTickAreaRight: Rectangle2D.Float =
      new Rectangle2D.Float(chartWidth - yTickMarginRight, xTickMargin + plotMargin, yTickMarginRight, chartHeight - xTickMargin - plotMargin)

    def origin: Point2D.Float = new Point2D.Float(yTickMarginLeft + plotMargin, xTickMargin + plotMargin)
  }

  object Layout {

    private def orDefault(value: Int, defaultValue: Int): Int = if (value == -1) defaultValue else value

    /** Basic set of layout options */
    def basic(chartWidth: Int, chartHeight: Int, xTickMargin: Int = -1, yTickMarginLeft: Int = -1, yTickMarginRight: Int = -1, plotMargin: Int = -1): Layout = {
      val xtm = orDefault(xTickMargin, Math.min(chartHeight/5, 50))
      val ytml = orDefault(yTickMarginLeft, Math.min(chartWidth/5, 50))
      val ytmr = orDefault(yTickMarginRight, 0)
      val pm = orDefault(plotMargin, Math.min(chartWidth/10, 5))
      Layout(chartWidth, chartHeight, xtm, ytml, ytmr, pm)
    }
  }

  case class Font (
    family: String,
    baseSize: Float,
    sizeIncrement: Float
   )

  object Font {
    def basic(family: String = "Arial", baseSize: Float = 14f, sizeIncrement: Float = 1.5f): Font = {
      Font(family, baseSize, sizeIncrement)
    }
  }

}
