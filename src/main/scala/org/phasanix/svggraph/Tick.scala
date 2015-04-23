package org.phasanix.svggraph

import java.text.SimpleDateFormat
import java.util.{Calendar, Date}

import org.phasanix.svggraph.Axis.ScaleAxis

/**
 * Definitions for ticks and tick boundaries
 */

object Tick {

  import Helper._

  type Fmt = Double => String

  class DateTickFmt(fmt1: String, fmt2: String) extends Function1[Double, String] {
    val dfmt1 = new SimpleDateFormat(fmt1)
    val dfmt2 = new SimpleDateFormat(fmt2)
    private[this] var prevYear = -1
    private[this] val cal = Calendar.getInstance() // TODO: java.time or datemath alternative

    def apply(d: Double): String = {
      cal.setTimeInMillis(d.toLong)
      val yr = cal.get(Calendar.YEAR)
      val ret = if (yr == prevYear && prevYear != -1) {
        dfmt1.format(new Date(d.toLong))
      } else {
        dfmt2.format(new Date(d.toLong))
      }
      prevYear = yr
      ret
    }
  }

  /** Date axis formatter, with optional formatter for Jan, so that we
    *  can display the year only when a year boundary is crossed. */
  def dateFmt(fmtStr: String): Fmt = {
    val fmt = new SimpleDateFormat(fmtStr);
    { d: Double => fmt.format(new Date(d.toLong)) }
  }

  val dateFmt_MY = dateFmt("MMM-yy")
  val dateFmt_DM = dateFmt("dd MMM")
  val dateFmt_DMY = dateFmt("dd MMM yy")
  val defaultDateFmt = new DateTickFmt("dd MMM", "dd MMM yy")

  val numFmt_0dp = { v: Double => fmt(v, 0) }
  val numFmt_2dp = { v: Double => fmt(v, 2) }

  def defaultFmt(isDate: Boolean): Double => String = if (isDate) defaultDateFmt else numFmt_0dp

  /**
   * Generate a series of tick-boundary values
   */
  def values(opts: Options, axis: ScaleAxis): Seq[Double] = {

    // estimate number of ticks to fit given range, round down to nearest 5,
    // but with a minimum of 3.
    val rawTickCount = {
      val x = ((axis.length.toInt / opts.draw.pixelsPerTick) / 5) * 5
      if (x == 0) 3 else x
    }

    axis.tickIntervals(rawTickCount).toSeq
  }
}
