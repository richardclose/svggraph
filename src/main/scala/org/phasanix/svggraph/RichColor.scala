package org.phasanix.svggraph
import java.awt.Color

import scala.language.implicitConversions

/**
 * Utility methods for handling Color.
 */
class RichColorOps(val value: Color) extends AnyVal {

  def setAlpha(alpha: Double): Color =
    new Color(value.getRed, value.getGreen, value.getBlue, (255 * alpha).toInt)

  def opaque: Color = if (value.getAlpha == 255) value else setAlpha(1.0)

  def asRgb: String = RichColor.colorAsRgb(value)

  def asHex: String = RichColor.colorAsHex(value)

  def or(color: Color): Color =
    if (value == RichColor.Nil) color else value
}

object RichColor extends X11Colors {

  implicit def color2richcolor(color: Color): RichColorOps = apply(color)

  val Transparent: Color = new Color(0, 0, 0, 0)

  val Nil: Color = new Color(12, 34, 56, 0)

  def apply(color: Color): RichColorOps = new RichColorOps(color)

  private val rxShortHex = """#([0-9A-F])([0-9A-F])([0-9A-F])""".r
  private val rxLongHex = """#([0-9A-F]{2})([0-9A-F]{2})([0-9A-F]{2})""".r
  private val rxRgb = """rgb\(\s*(\d+),\s*(\d+),\s*(\d+)\)""".r
  private val rxRgba = """rgba\(\s*(\d+),\s*(\d+),\s*(\d+),\s*(0.\d{1,3})\)""".r

  /**
   * Parse a string in one of short hex (e.g. #DCB), long hex (e.g. #DDCCBB),
   * rgb (e.g. rgb(100,120,130)) or rgba (e.g. rgba(100,120,130,0.5)
   */
  def parse(s: String): Option[Color] = {
    import java.lang.Integer.{ parseInt => pi }
    import java.lang.Float.{ parseFloat => pf }
    val c = s match {
      case rxShortHex(r, g, b) => new Color(pi(r + r, 16), pi(g + g, 16), pi(b + b, 16), 255)
      case rxLongHex(r, g, b) => new Color(pi(r, 16), pi(g, 16), pi(b, 16), 255)
      case rxRgb(r, g, b) => new Color(pi(r), pi(g), pi(b), 255)
      case rxRgba(r, g, b, a) =>
        val alpha = pf(a)
        val alphaInt = if (alpha == 0.0) 0 else (256f * alpha).toInt - 1
        new Color(pi(r), pi(g), pi(b), alphaInt)
      case _ => null
    }
    Option(c)
  }

  /**
   * Convenience method, supplies default colour if parse fails
   */
  def parseOrElse(s: String, defaultColor: Color): Color = {
    parse(s).getOrElse(defaultColor)
  }

  /**
   * represent color as rgb string, or rgba if alpha is present
   */
  def encodeAsRgb(sb: StringBuilder, color: Color) {
    val alpha = color.getAlpha
    val hasAlpha = alpha < 255
    sb.append(if (hasAlpha) "rgba(" else "rgb(")
      .append(color.getRed).append(',')
      .append(color.getGreen).append(',')
      .append(color.getBlue)
    if (hasAlpha) {
      val s = "%.6f".format((alpha+1)/256.0).reverse.dropWhile(_ == '0').reverse // Remove trailing zeros
      sb.append(',').append(s)
    }

    sb.append(')')
  }

  /** represent color as hex string, ignoring alpha */
  def encodeAsHex(sb: StringBuilder, color: Color) {
    sb.append('#')
    val s = "%6X".format(color.getRGB)
    sb.append(s.substring(2))
  }

  /** Color as HTML Hex string (e.g. "#12BCA3" */
  def colorAsHex(color: Color) = {
    val sb = new StringBuilder(7)
    encodeAsHex(sb, color)
    sb.toString()
  }

  /** Color as rgb or rgba value */
  def colorAsRgb(color: Color) = {
    val sb = new StringBuilder(20)
    encodeAsRgb(sb, color)
    sb.toString()
  }

  /**
   * Generate colours by interpolating the requested number of colours between
   * the given colours. Return range including original colours, or range(WHITE,BLACK,count)
   * if the strings don't parse.
   */
  def range(from: String, to: String, count: Int): Seq[Color] = {
    val maybeRange = for (c1 <- parse(from); c2 <- parse(to)) yield range(c1, c2, count)
    maybeRange.getOrElse(range(Color.WHITE, Color.BLACK, count))
  }

  /**
   * Generate colours by interpolating the requested number of colours between
   * the given colours. Return range includes original colours.
   */
  def range(from: Color, to: Color, count: Int): Seq[Color] = {
    def arr(c: Color) = Seq(c.getRed, c.getGreen, c.getBlue, c.getAlpha)
    val arr1 = arr(from)
    val arr2 = arr(to)
    val steps = arr1.zip(arr2).map(e => (e._1 - e._2) / (count + 1))
    val colours = 1 to count map { i =>
      val xs = arr1.zip(steps).map(e => e._1 - i * e._2)
      new Color(xs(0), xs(1), xs(2), xs(3))
    }

    from +: colours :+ to
  }

  /**
   * Generate a Function1[Double, Color] for converting a value to a mapped
   * color from the given range of colors.
   */
  def rangeMapperLinear(colors: Seq[Color], values: Seq[Double]): Double => Color = {
    val (min, max) = (values.min, values.max)

    { value: Double =>
      val mapped = (colors.length * (value-min)/(max-min)).toInt
      val index = if (mapped < 0) 0 else if (mapped >= colors.length) colors.length - 1 else mapped
      colors(index)
    }
  }

  private def _interp(from: Int, to: Int, dist: Double) = {
    (from + (to - from) * dist).toInt
  }

  /**
   * Linearly interpolate between two colours
   */
  def interp(from: Color, to: Color, distance: Double) = {
    val dist = Math.max(0.0, Math.min(1.0, distance))
    new Color(
      _interp(from.getRed, to.getRed, dist),
      _interp(from.getGreen, to.getGreen, dist),
      _interp(from.getBlue, to.getBlue, dist),
      _interp(from.getAlpha, to.getAlpha, dist))
  }
}

/**
 * Converted from /etc/X11/rgb.txt
 */
trait X11Colors {
  val AliceBlue = new Color(240, 248, 255)
  val AntiqueWhite = new Color(250, 235, 215)
  val AntiqueWhite1 = new Color(255, 239, 219)
  val AntiqueWhite2 = new Color(238, 223, 204)
  val AntiqueWhite3 = new Color(205, 192, 176)
  val AntiqueWhite4 = new Color(139, 131, 120)
  val Aquamarine = new Color(127, 255, 212)
  val Aquamarine1 = new Color(127, 255, 212)
  val Aquamarine2 = new Color(118, 238, 198)
  val Aquamarine3 = new Color(102, 205, 170)
  val Aquamarine4 = new Color(69, 139, 116)
  val Azure = new Color(240, 255, 255)
  val Azure1 = new Color(240, 255, 255)
  val Azure2 = new Color(224, 238, 238)
  val Azure3 = new Color(193, 205, 205)
  val Azure4 = new Color(131, 139, 139)
  val Beige = new Color(245, 245, 220)
  val Bisque = new Color(255, 228, 196)
  val Bisque1 = new Color(255, 228, 196)
  val Bisque2 = new Color(238, 213, 183)
  val Bisque3 = new Color(205, 183, 158)
  val Bisque4 = new Color(139, 125, 107)
  val Black = new Color(0, 0, 0)
  val BlanchedAlmond = new Color(255, 235, 205)
  val Blue = new Color(0, 0, 255)
  val Blue1 = new Color(0, 0, 255)
  val Blue2 = new Color(0, 0, 238)
  val Blue3 = new Color(0, 0, 205)
  val Blue4 = new Color(0, 0, 139)
  val BlueViolet = new Color(138, 43, 226)
  val Brown = new Color(165, 42, 42)
  val Brown1 = new Color(255, 64, 64)
  val Brown2 = new Color(238, 59, 59)
  val Brown3 = new Color(205, 51, 51)
  val Brown4 = new Color(139, 35, 35)
  val Burlywood = new Color(222, 184, 135)
  val Burlywood1 = new Color(255, 211, 155)
  val Burlywood2 = new Color(238, 197, 145)
  val Burlywood3 = new Color(205, 170, 125)
  val Burlywood4 = new Color(139, 115, 85)
  val CadetBlue = new Color(95, 158, 160)
  val CadetBlue1 = new Color(152, 245, 255)
  val CadetBlue2 = new Color(142, 229, 238)
  val CadetBlue3 = new Color(122, 197, 205)
  val CadetBlue4 = new Color(83, 134, 139)
  val Chartreuse = new Color(127, 255, 0)
  val Chartreuse1 = new Color(127, 255, 0)
  val Chartreuse2 = new Color(118, 238, 0)
  val Chartreuse3 = new Color(102, 205, 0)
  val Chartreuse4 = new Color(69, 139, 0)
  val Chocolate = new Color(210, 105, 30)
  val Chocolate1 = new Color(255, 127, 36)
  val Chocolate2 = new Color(238, 118, 33)
  val Chocolate3 = new Color(205, 102, 29)
  val Chocolate4 = new Color(139, 69, 19)
  val Coral = new Color(255, 127, 80)
  val Coral1 = new Color(255, 114, 86)
  val Coral2 = new Color(238, 106, 80)
  val Coral3 = new Color(205, 91, 69)
  val Coral4 = new Color(139, 62, 47)
  val CornflowerBlue = new Color(100, 149, 237)
  val Cornsilk = new Color(255, 248, 220)
  val Cornsilk1 = new Color(255, 248, 220)
  val Cornsilk2 = new Color(238, 232, 205)
  val Cornsilk3 = new Color(205, 200, 177)
  val Cornsilk4 = new Color(139, 136, 120)
  val Cyan = new Color(0, 255, 255)
  val Cyan1 = new Color(0, 255, 255)
  val Cyan2 = new Color(0, 238, 238)
  val Cyan3 = new Color(0, 205, 205)
  val Cyan4 = new Color(0, 139, 139)
  val DarkBlue = new Color(0, 0, 139)
  val DarkCyan = new Color(0, 139, 139)
  val DarkGoldenrod = new Color(184, 134, 11)
  val DarkGoldenrod1 = new Color(255, 185, 15)
  val DarkGoldenrod2 = new Color(238, 173, 14)
  val DarkGoldenrod3 = new Color(205, 149, 12)
  val DarkGoldenrod4 = new Color(139, 101, 8)
  val DarkGray = new Color(169, 169, 169)
  val DarkGreen = new Color(0, 100, 0)
  val DarkKhaki = new Color(189, 183, 107)
  val DarkMagenta = new Color(139, 0, 139)
  val DarkOliveGreen = new Color(85, 107, 47)
  val DarkOliveGreen1 = new Color(202, 255, 112)
  val DarkOliveGreen2 = new Color(188, 238, 104)
  val DarkOliveGreen3 = new Color(162, 205, 90)
  val DarkOliveGreen4 = new Color(110, 139, 61)
  val DarkOrange = new Color(255, 140, 0)
  val DarkOrange1 = new Color(255, 127, 0)
  val DarkOrange2 = new Color(238, 118, 0)
  val DarkOrange3 = new Color(205, 102, 0)
  val DarkOrange4 = new Color(139, 69, 0)
  val DarkOrchid = new Color(153, 50, 204)
  val DarkOrchid1 = new Color(191, 62, 255)
  val DarkOrchid2 = new Color(178, 58, 238)
  val DarkOrchid3 = new Color(154, 50, 205)
  val DarkOrchid4 = new Color(104, 34, 139)
  val DarkRed = new Color(139, 0, 0)
  val DarkSalmon = new Color(233, 150, 122)
  val DarkSeaGreen = new Color(143, 188, 143)
  val DarkSeaGreen1 = new Color(193, 255, 193)
  val DarkSeaGreen2 = new Color(180, 238, 180)
  val DarkSeaGreen3 = new Color(155, 205, 155)
  val DarkSeaGreen4 = new Color(105, 139, 105)
  val DarkSlateBlue = new Color(72, 61, 139)
  val DarkSlateGray = new Color(47, 79, 79)
  val DarkSlateGray1 = new Color(151, 255, 255)
  val DarkSlateGray2 = new Color(141, 238, 238)
  val DarkSlateGray3 = new Color(121, 205, 205)
  val DarkSlateGray4 = new Color(82, 139, 139)
  val DarkTurquoise = new Color(0, 206, 209)
  val DarkViolet = new Color(148, 0, 211)
  val DebianRed = new Color(215, 7, 81)
  val DeepPink = new Color(255, 20, 147)
  val DeepPink1 = new Color(255, 20, 147)
  val DeepPink2 = new Color(238, 18, 137)
  val DeepPink3 = new Color(205, 16, 118)
  val DeepPink4 = new Color(139, 10, 80)
  val DeepSkyBlue = new Color(0, 191, 255)
  val DeepSkyBlue1 = new Color(0, 191, 255)
  val DeepSkyBlue2 = new Color(0, 178, 238)
  val DeepSkyBlue3 = new Color(0, 154, 205)
  val DeepSkyBlue4 = new Color(0, 104, 139)
  val DimGray = new Color(105, 105, 105)
  val DodgerBlue = new Color(30, 144, 255)
  val DodgerBlue1 = new Color(30, 144, 255)
  val DodgerBlue2 = new Color(28, 134, 238)
  val DodgerBlue3 = new Color(24, 116, 205)
  val DodgerBlue4 = new Color(16, 78, 139)
  val Firebrick = new Color(178, 34, 34)
  val Firebrick1 = new Color(255, 48, 48)
  val Firebrick2 = new Color(238, 44, 44)
  val Firebrick3 = new Color(205, 38, 38)
  val Firebrick4 = new Color(139, 26, 26)
  val FloralWhite = new Color(255, 250, 240)
  val ForestGreen = new Color(34, 139, 34)
  val Gainsboro = new Color(220, 220, 220)
  val GhostWhite = new Color(248, 248, 255)
  val Gold = new Color(255, 215, 0)
  val Gold1 = new Color(255, 215, 0)
  val Gold2 = new Color(238, 201, 0)
  val Gold3 = new Color(205, 173, 0)
  val Gold4 = new Color(139, 117, 0)
  val Goldenrod = new Color(218, 165, 32)
  val Goldenrod1 = new Color(255, 193, 37)
  val Goldenrod2 = new Color(238, 180, 34)
  val Goldenrod3 = new Color(205, 155, 29)
  val Goldenrod4 = new Color(139, 105, 20)
  val Gray = new Color(190, 190, 190)
  val Gray0 = new Color(0, 0, 0)
  val Gray1 = new Color(3, 3, 3)
  val Gray10 = new Color(26, 26, 26)
  val Gray100 = new Color(255, 255, 255)
  val Gray11 = new Color(28, 28, 28)
  val Gray12 = new Color(31, 31, 31)
  val Gray13 = new Color(33, 33, 33)
  val Gray14 = new Color(36, 36, 36)
  val Gray15 = new Color(38, 38, 38)
  val Gray16 = new Color(41, 41, 41)
  val Gray17 = new Color(43, 43, 43)
  val Gray18 = new Color(46, 46, 46)
  val Gray19 = new Color(48, 48, 48)
  val Gray2 = new Color(5, 5, 5)
  val Gray20 = new Color(51, 51, 51)
  val Gray21 = new Color(54, 54, 54)
  val Gray22 = new Color(56, 56, 56)
  val Gray23 = new Color(59, 59, 59)
  val Gray24 = new Color(61, 61, 61)
  val Gray25 = new Color(64, 64, 64)
  val Gray26 = new Color(66, 66, 66)
  val Gray27 = new Color(69, 69, 69)
  val Gray28 = new Color(71, 71, 71)
  val Gray29 = new Color(74, 74, 74)
  val Gray3 = new Color(8, 8, 8)
  val Gray30 = new Color(77, 77, 77)
  val Gray31 = new Color(79, 79, 79)
  val Gray32 = new Color(82, 82, 82)
  val Gray33 = new Color(84, 84, 84)
  val Gray34 = new Color(87, 87, 87)
  val Gray35 = new Color(89, 89, 89)
  val Gray36 = new Color(92, 92, 92)
  val Gray37 = new Color(94, 94, 94)
  val Gray38 = new Color(97, 97, 97)
  val Gray39 = new Color(99, 99, 99)
  val Gray4 = new Color(10, 10, 10)
  val Gray40 = new Color(102, 102, 102)
  val Gray41 = new Color(105, 105, 105)
  val Gray42 = new Color(107, 107, 107)
  val Gray43 = new Color(110, 110, 110)
  val Gray44 = new Color(112, 112, 112)
  val Gray45 = new Color(115, 115, 115)
  val Gray46 = new Color(117, 117, 117)
  val Gray47 = new Color(120, 120, 120)
  val Gray48 = new Color(122, 122, 122)
  val Gray49 = new Color(125, 125, 125)
  val Gray5 = new Color(13, 13, 13)
  val Gray50 = new Color(127, 127, 127)
  val Gray51 = new Color(130, 130, 130)
  val Gray52 = new Color(133, 133, 133)
  val Gray53 = new Color(135, 135, 135)
  val Gray54 = new Color(138, 138, 138)
  val Gray55 = new Color(140, 140, 140)
  val Gray56 = new Color(143, 143, 143)
  val Gray57 = new Color(145, 145, 145)
  val Gray58 = new Color(148, 148, 148)
  val Gray59 = new Color(150, 150, 150)
  val Gray6 = new Color(15, 15, 15)
  val Gray60 = new Color(153, 153, 153)
  val Gray61 = new Color(156, 156, 156)
  val Gray62 = new Color(158, 158, 158)
  val Gray63 = new Color(161, 161, 161)
  val Gray64 = new Color(163, 163, 163)
  val Gray65 = new Color(166, 166, 166)
  val Gray66 = new Color(168, 168, 168)
  val Gray67 = new Color(171, 171, 171)
  val Gray68 = new Color(173, 173, 173)
  val Gray69 = new Color(176, 176, 176)
  val Gray7 = new Color(18, 18, 18)
  val Gray70 = new Color(179, 179, 179)
  val Gray71 = new Color(181, 181, 181)
  val Gray72 = new Color(184, 184, 184)
  val Gray73 = new Color(186, 186, 186)
  val Gray74 = new Color(189, 189, 189)
  val Gray75 = new Color(191, 191, 191)
  val Gray76 = new Color(194, 194, 194)
  val Gray77 = new Color(196, 196, 196)
  val Gray78 = new Color(199, 199, 199)
  val Gray79 = new Color(201, 201, 201)
  val Gray8 = new Color(20, 20, 20)
  val Gray80 = new Color(204, 204, 204)
  val Gray81 = new Color(207, 207, 207)
  val Gray82 = new Color(209, 209, 209)
  val Gray83 = new Color(212, 212, 212)
  val Gray84 = new Color(214, 214, 214)
  val Gray85 = new Color(217, 217, 217)
  val Gray86 = new Color(219, 219, 219)
  val Gray87 = new Color(222, 222, 222)
  val Gray88 = new Color(224, 224, 224)
  val Gray89 = new Color(227, 227, 227)
  val Gray9 = new Color(23, 23, 23)
  val Gray90 = new Color(229, 229, 229)
  val Gray91 = new Color(232, 232, 232)
  val Gray92 = new Color(235, 235, 235)
  val Gray93 = new Color(237, 237, 237)
  val Gray94 = new Color(240, 240, 240)
  val Gray95 = new Color(242, 242, 242)
  val Gray96 = new Color(245, 245, 245)
  val Gray97 = new Color(247, 247, 247)
  val Gray98 = new Color(250, 250, 250)
  val Gray99 = new Color(252, 252, 252)
  val Green = new Color(0, 255, 0)
  val Green1 = new Color(0, 255, 0)
  val Green2 = new Color(0, 238, 0)
  val Green3 = new Color(0, 205, 0)
  val Green4 = new Color(0, 139, 0)
  val GreenYellow = new Color(173, 255, 47)
  val Honeydew = new Color(240, 255, 240)
  val Honeydew1 = new Color(240, 255, 240)
  val Honeydew2 = new Color(224, 238, 224)
  val Honeydew3 = new Color(193, 205, 193)
  val Honeydew4 = new Color(131, 139, 131)
  val HotPink = new Color(255, 105, 180)
  val HotPink1 = new Color(255, 110, 180)
  val HotPink2 = new Color(238, 106, 167)
  val HotPink3 = new Color(205, 96, 144)
  val HotPink4 = new Color(139, 58, 98)
  val IndianRed = new Color(205, 92, 92)
  val IndianRed1 = new Color(255, 106, 106)
  val IndianRed2 = new Color(238, 99, 99)
  val IndianRed3 = new Color(205, 85, 85)
  val IndianRed4 = new Color(139, 58, 58)
  val Ivory = new Color(255, 255, 240)
  val Ivory1 = new Color(255, 255, 240)
  val Ivory2 = new Color(238, 238, 224)
  val Ivory3 = new Color(205, 205, 193)
  val Ivory4 = new Color(139, 139, 131)
  val Khaki = new Color(240, 230, 140)
  val Khaki1 = new Color(255, 246, 143)
  val Khaki2 = new Color(238, 230, 133)
  val Khaki3 = new Color(205, 198, 115)
  val Khaki4 = new Color(139, 134, 78)
  val Lavender = new Color(230, 230, 250)
  val LavenderBlush = new Color(255, 240, 245)
  val LavenderBlush1 = new Color(255, 240, 245)
  val LavenderBlush2 = new Color(238, 224, 229)
  val LavenderBlush3 = new Color(205, 193, 197)
  val LavenderBlush4 = new Color(139, 131, 134)
  val LawnGreen = new Color(124, 252, 0)
  val LemonChiffon = new Color(255, 250, 205)
  val LemonChiffon1 = new Color(255, 250, 205)
  val LemonChiffon2 = new Color(238, 233, 191)
  val LemonChiffon3 = new Color(205, 201, 165)
  val LemonChiffon4 = new Color(139, 137, 112)
  val LightBlue = new Color(173, 216, 230)
  val LightBlue1 = new Color(191, 239, 255)
  val LightBlue2 = new Color(178, 223, 238)
  val LightBlue3 = new Color(154, 192, 205)
  val LightBlue4 = new Color(104, 131, 139)
  val LightCoral = new Color(240, 128, 128)
  val LightCyan = new Color(224, 255, 255)
  val LightCyan1 = new Color(224, 255, 255)
  val LightCyan2 = new Color(209, 238, 238)
  val LightCyan3 = new Color(180, 205, 205)
  val LightCyan4 = new Color(122, 139, 139)
  val LightGoldenrod = new Color(238, 221, 130)
  val LightGoldenrod1 = new Color(255, 236, 139)
  val LightGoldenrod2 = new Color(238, 220, 130)
  val LightGoldenrod3 = new Color(205, 190, 112)
  val LightGoldenrod4 = new Color(139, 129, 76)
  val LightGoldenrodYellow = new Color(250, 250, 210)
  val LightGray = new Color(211, 211, 211)
  val LightGreen = new Color(144, 238, 144)
  val LightPink = new Color(255, 182, 193)
  val LightPink1 = new Color(255, 174, 185)
  val LightPink2 = new Color(238, 162, 173)
  val LightPink3 = new Color(205, 140, 149)
  val LightPink4 = new Color(139, 95, 101)
  val LightSalmon = new Color(255, 160, 122)
  val LightSalmon1 = new Color(255, 160, 122)
  val LightSalmon2 = new Color(238, 149, 114)
  val LightSalmon3 = new Color(205, 129, 98)
  val LightSalmon4 = new Color(139, 87, 66)
  val LightSeaGreen = new Color(32, 178, 170)
  val LightSkyBlue = new Color(135, 206, 250)
  val LightSkyBlue1 = new Color(176, 226, 255)
  val LightSkyBlue2 = new Color(164, 211, 238)
  val LightSkyBlue3 = new Color(141, 182, 205)
  val LightSkyBlue4 = new Color(96, 123, 139)
  val LightSlateBlue = new Color(132, 112, 255)
  val LightSlateGray = new Color(119, 136, 153)
  val LightSteelBlue = new Color(176, 196, 222)
  val LightSteelBlue1 = new Color(202, 225, 255)
  val LightSteelBlue2 = new Color(188, 210, 238)
  val LightSteelBlue3 = new Color(162, 181, 205)
  val LightSteelBlue4 = new Color(110, 123, 139)
  val LightYellow = new Color(255, 255, 224)
  val LightYellow1 = new Color(255, 255, 224)
  val LightYellow2 = new Color(238, 238, 209)
  val LightYellow3 = new Color(205, 205, 180)
  val LightYellow4 = new Color(139, 139, 122)
  val LimeGreen = new Color(50, 205, 50)
  val Linen = new Color(250, 240, 230)
  val Magenta = new Color(255, 0, 255)
  val Magenta1 = new Color(255, 0, 255)
  val Magenta2 = new Color(238, 0, 238)
  val Magenta3 = new Color(205, 0, 205)
  val Magenta4 = new Color(139, 0, 139)
  val Maroon = new Color(176, 48, 96)
  val Maroon1 = new Color(255, 52, 179)
  val Maroon2 = new Color(238, 48, 167)
  val Maroon3 = new Color(205, 41, 144)
  val Maroon4 = new Color(139, 28, 98)
  val MediumAquamarine = new Color(102, 205, 170)
  val MediumBlue = new Color(0, 0, 205)
  val MediumOrchid = new Color(186, 85, 211)
  val MediumOrchid1 = new Color(224, 102, 255)
  val MediumOrchid2 = new Color(209, 95, 238)
  val MediumOrchid3 = new Color(180, 82, 205)
  val MediumOrchid4 = new Color(122, 55, 139)
  val MediumPurple = new Color(147, 112, 219)
  val MediumPurple1 = new Color(171, 130, 255)
  val MediumPurple2 = new Color(159, 121, 238)
  val MediumPurple3 = new Color(137, 104, 205)
  val MediumPurple4 = new Color(93, 71, 139)
  val MediumSeaGreen = new Color(60, 179, 113)
  val MediumSlateBlue = new Color(123, 104, 238)
  val MediumSpringGreen = new Color(0, 250, 154)
  val MediumTurquoise = new Color(72, 209, 204)
  val MediumVioletRed = new Color(199, 21, 133)
  val MidnightBlue = new Color(25, 25, 112)
  val MintCream = new Color(245, 255, 250)
  val MistyRose = new Color(255, 228, 225)
  val MistyRose1 = new Color(255, 228, 225)
  val MistyRose2 = new Color(238, 213, 210)
  val MistyRose3 = new Color(205, 183, 181)
  val MistyRose4 = new Color(139, 125, 123)
  val Moccasin = new Color(255, 228, 181)
  val NavajoWhite = new Color(255, 222, 173)
  val NavajoWhite1 = new Color(255, 222, 173)
  val NavajoWhite2 = new Color(238, 207, 161)
  val NavajoWhite3 = new Color(205, 179, 139)
  val NavajoWhite4 = new Color(139, 121, 94)
  val Navy = new Color(0, 0, 128)
  val NavyBlue = new Color(0, 0, 128)
  val OldLace = new Color(253, 245, 230)
  val OliveDrab = new Color(107, 142, 35)
  val OliveDrab1 = new Color(192, 255, 62)
  val OliveDrab2 = new Color(179, 238, 58)
  val OliveDrab3 = new Color(154, 205, 50)
  val OliveDrab4 = new Color(105, 139, 34)
  val Orange = new Color(255, 165, 0)
  val Orange1 = new Color(255, 165, 0)
  val Orange2 = new Color(238, 154, 0)
  val Orange3 = new Color(205, 133, 0)
  val Orange4 = new Color(139, 90, 0)
  val OrangeRed = new Color(255, 69, 0)
  val OrangeRed1 = new Color(255, 69, 0)
  val OrangeRed2 = new Color(238, 64, 0)
  val OrangeRed3 = new Color(205, 55, 0)
  val OrangeRed4 = new Color(139, 37, 0)
  val Orchid = new Color(218, 112, 214)
  val Orchid1 = new Color(255, 131, 250)
  val Orchid2 = new Color(238, 122, 233)
  val Orchid3 = new Color(205, 105, 201)
  val Orchid4 = new Color(139, 71, 137)
  val PaleGoldenrod = new Color(238, 232, 170)
  val PaleGreen = new Color(152, 251, 152)
  val PaleGreen1 = new Color(154, 255, 154)
  val PaleGreen2 = new Color(144, 238, 144)
  val PaleGreen3 = new Color(124, 205, 124)
  val PaleGreen4 = new Color(84, 139, 84)
  val PaleTurquoise = new Color(175, 238, 238)
  val PaleTurquoise1 = new Color(187, 255, 255)
  val PaleTurquoise2 = new Color(174, 238, 238)
  val PaleTurquoise3 = new Color(150, 205, 205)
  val PaleTurquoise4 = new Color(102, 139, 139)
  val PaleVioletRed = new Color(219, 112, 147)
  val PaleVioletRed1 = new Color(255, 130, 171)
  val PaleVioletRed2 = new Color(238, 121, 159)
  val PaleVioletRed3 = new Color(205, 104, 137)
  val PaleVioletRed4 = new Color(139, 71, 93)
  val PapayaWhip = new Color(255, 239, 213)
  val PeachPuff = new Color(255, 218, 185)
  val PeachPuff1 = new Color(255, 218, 185)
  val PeachPuff2 = new Color(238, 203, 173)
  val PeachPuff3 = new Color(205, 175, 149)
  val PeachPuff4 = new Color(139, 119, 101)
  val Peru = new Color(205, 133, 63)
  val Pink = new Color(255, 192, 203)
  val Pink1 = new Color(255, 181, 197)
  val Pink2 = new Color(238, 169, 184)
  val Pink3 = new Color(205, 145, 158)
  val Pink4 = new Color(139, 99, 108)
  val Plum = new Color(221, 160, 221)
  val Plum1 = new Color(255, 187, 255)
  val Plum2 = new Color(238, 174, 238)
  val Plum3 = new Color(205, 150, 205)
  val Plum4 = new Color(139, 102, 139)
  val PowderBlue = new Color(176, 224, 230)
  val Purple = new Color(160, 32, 240)
  val Purple1 = new Color(155, 48, 255)
  val Purple2 = new Color(145, 44, 238)
  val Purple3 = new Color(125, 38, 205)
  val Purple4 = new Color(85, 26, 139)
  val Red = new Color(255, 0, 0)
  val Red1 = new Color(255, 0, 0)
  val Red2 = new Color(238, 0, 0)
  val Red3 = new Color(205, 0, 0)
  val Red4 = new Color(139, 0, 0)
  val RosyBrown = new Color(188, 143, 143)
  val RosyBrown1 = new Color(255, 193, 193)
  val RosyBrown2 = new Color(238, 180, 180)
  val RosyBrown3 = new Color(205, 155, 155)
  val RosyBrown4 = new Color(139, 105, 105)
  val RoyalBlue = new Color(65, 105, 225)
  val RoyalBlue1 = new Color(72, 118, 255)
  val RoyalBlue2 = new Color(67, 110, 238)
  val RoyalBlue3 = new Color(58, 95, 205)
  val RoyalBlue4 = new Color(39, 64, 139)
  val SaddleBrown = new Color(139, 69, 19)
  val Salmon = new Color(250, 128, 114)
  val Salmon1 = new Color(255, 140, 105)
  val Salmon2 = new Color(238, 130, 98)
  val Salmon3 = new Color(205, 112, 84)
  val Salmon4 = new Color(139, 76, 57)
  val SandyBrown = new Color(244, 164, 96)
  val SeaGreen = new Color(46, 139, 87)
  val SeaGreen1 = new Color(84, 255, 159)
  val SeaGreen2 = new Color(78, 238, 148)
  val SeaGreen3 = new Color(67, 205, 128)
  val SeaGreen4 = new Color(46, 139, 87)
  val Seashell = new Color(255, 245, 238)
  val Seashell1 = new Color(255, 245, 238)
  val Seashell2 = new Color(238, 229, 222)
  val Seashell3 = new Color(205, 197, 191)
  val Seashell4 = new Color(139, 134, 130)
  val Sienna = new Color(160, 82, 45)
  val Sienna1 = new Color(255, 130, 71)
  val Sienna2 = new Color(238, 121, 66)
  val Sienna3 = new Color(205, 104, 57)
  val Sienna4 = new Color(139, 71, 38)
  val SkyBlue = new Color(135, 206, 235)
  val SkyBlue1 = new Color(135, 206, 255)
  val SkyBlue2 = new Color(126, 192, 238)
  val SkyBlue3 = new Color(108, 166, 205)
  val SkyBlue4 = new Color(74, 112, 139)
  val SlateBlue = new Color(106, 90, 205)
  val SlateBlue1 = new Color(131, 111, 255)
  val SlateBlue2 = new Color(122, 103, 238)
  val SlateBlue3 = new Color(105, 89, 205)
  val SlateBlue4 = new Color(71, 60, 139)
  val SlateGray = new Color(112, 128, 144)
  val SlateGray1 = new Color(198, 226, 255)
  val SlateGray2 = new Color(185, 211, 238)
  val SlateGray3 = new Color(159, 182, 205)
  val SlateGray4 = new Color(108, 123, 139)
  val Snow = new Color(255, 250, 250)
  val Snow1 = new Color(255, 250, 250)
  val Snow2 = new Color(238, 233, 233)
  val Snow3 = new Color(205, 201, 201)
  val Snow4 = new Color(139, 137, 137)
  val SpringGreen = new Color(0, 255, 127)
  val SpringGreen1 = new Color(0, 255, 127)
  val SpringGreen2 = new Color(0, 238, 118)
  val SpringGreen3 = new Color(0, 205, 102)
  val SpringGreen4 = new Color(0, 139, 69)
  val SteelBlue = new Color(70, 130, 180)
  val SteelBlue1 = new Color(99, 184, 255)
  val SteelBlue2 = new Color(92, 172, 238)
  val SteelBlue3 = new Color(79, 148, 205)
  val SteelBlue4 = new Color(54, 100, 139)
  val Tan = new Color(210, 180, 140)
  val Tan1 = new Color(255, 165, 79)
  val Tan2 = new Color(238, 154, 73)
  val Tan3 = new Color(205, 133, 63)
  val Tan4 = new Color(139, 90, 43)
  val Thistle = new Color(216, 191, 216)
  val Thistle1 = new Color(255, 225, 255)
  val Thistle2 = new Color(238, 210, 238)
  val Thistle3 = new Color(205, 181, 205)
  val Thistle4 = new Color(139, 123, 139)
  val Tomato = new Color(255, 99, 71)
  val Tomato1 = new Color(255, 99, 71)
  val Tomato2 = new Color(238, 92, 66)
  val Tomato3 = new Color(205, 79, 57)
  val Tomato4 = new Color(139, 54, 38)
  val Turquoise = new Color(64, 224, 208)
  val Turquoise1 = new Color(0, 245, 255)
  val Turquoise2 = new Color(0, 229, 238)
  val Turquoise3 = new Color(0, 197, 205)
  val Turquoise4 = new Color(0, 134, 139)
  val Violet = new Color(238, 130, 238)
  val VioletRed = new Color(208, 32, 144)
  val VioletRed1 = new Color(255, 62, 150)
  val VioletRed2 = new Color(238, 58, 140)
  val VioletRed3 = new Color(205, 50, 120)
  val VioletRed4 = new Color(139, 34, 82)
  val Wheat = new Color(245, 222, 179)
  val Wheat1 = new Color(255, 231, 186)
  val Wheat2 = new Color(238, 216, 174)
  val Wheat3 = new Color(205, 186, 150)
  val Wheat4 = new Color(139, 126, 102)
  val White = new Color(255, 255, 255)
  val WhiteSmoke = new Color(245, 245, 245)
  val Yellow = new Color(255, 255, 0)
  val Yellow1 = new Color(255, 255, 0)
  val Yellow2 = new Color(238, 238, 0)
  val Yellow3 = new Color(205, 205, 0)
  val Yellow4 = new Color(139, 139, 0)
  val YellowGreen = new Color(154, 205, 50)
}