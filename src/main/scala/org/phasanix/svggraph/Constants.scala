package org.phasanix.svggraph

/**
 * Constants for charting
 */
trait Constants {

  trait Named {
    override def toString: String = this.getClass.getSimpleName.replace("$", "")
  }
  
  /**
   * Fill styles
   */
  sealed trait FillStyle extends Named
  object Solid extends FillStyle
  object Radial extends FillStyle
  object Vertical extends FillStyle
  object Horizontal extends FillStyle

  /**
   * Position (e.g. of graph legend).
   */
  sealed trait Corner extends Named
  object TopLeft extends Corner
  object TopRight extends Corner
  object BottomLeft extends Corner
  object BottomRight extends Corner
}

object Constants extends Constants
