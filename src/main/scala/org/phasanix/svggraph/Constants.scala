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

  /**
   * Adds particular attributes to an element
   */
  class AttrProvider(val name: String,  val value: String) {
    def apply(elt: xml.Elem): xml.Elem =
      if (value == null)
        elt
      else
        elt % new xml.UnprefixedAttribute(name, value, xml.Null)
  }

  /**
   * Text anchor, for alignment
   */
  sealed class TextAnchor(value: String) extends AttrProvider("text-anchor", value)

  object TextAnchor {
    object None extends TextAnchor(null: String)
    object Start extends TextAnchor("start")
    object Middle extends TextAnchor("middle")
    object End extends TextAnchor("end")
  }

}

object Constants extends Constants
