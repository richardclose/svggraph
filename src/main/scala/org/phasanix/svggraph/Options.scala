package org.phasanix.svggraph

import java.awt.geom.{Rectangle2D, Point2D}

/**
 * Configuration options.
 *
 * Measurements are all in px.
 */
object Options {

  case class Layout (
    chartHeight: Int,
    chartWidth: Int,
    xTickMargin: Int,
    yTickMarginLeft: Int,
    yTickMarginRight: Int,
    plotMargin: Int
  )

  object Layout {

    private def orDefault(value: Int, defaultValue: Int): Int = if (value == -1) defaultValue else value

    def medium(chartHeight: Int, chartWidth: Int, xTickMargin: Int = -1, yTickMarginLeft: Int = -1, yTickMarginRight: Int = -1, plotMargin: Int = -1): Layout = {
      val xtm = orDefault(xTickMargin, Math.min(chartHeight/5, 50))
      val ytml = orDefault(yTickMarginLeft, Math.min(chartWidth/5, 50))
      val ytmr = orDefault(yTickMarginRight, 0)
      val pm = orDefault(plotMargin, Math.min(chartWidth/10, 10))
      Layout(chartHeight, chartWidth, xtm, ytml, ytmr, pm)
    }
  }

}
