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
   * Text anchor, for horizontal alignment
   */
  class TextAnchor(value: String) extends AttrProvider("text-anchor", value)

  object TextAnchor {
    val Nil = new TextAnchor(null: String)
    val Start = new TextAnchor("start")
    val Middle = new TextAnchor("middle")
    val End = new TextAnchor("end")
  }


  sealed class BaselineAlignment(value: String) extends AttrProvider("alignment-baseline", value)

  object BaselineAlignment {
    val Nil = new BaselineAlignment(null: String)
    val Middle = new BaselineAlignment("middle")
  }

}

object Constants extends Constants
