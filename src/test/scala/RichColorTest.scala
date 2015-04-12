import org.phasanix.svggraph.RichColor
import org.scalatest.{Matchers, FlatSpec}

import org.phasanix.svggraph.RichColor._
import java.awt.Color

/**
 * Test suite for RichColor
 */
class RichColorTest  extends FlatSpec with Matchers {

  def isBetween(color1: Color, color2: Color, mid: Color): Boolean = {
    def between(c1: Int, c2: Int, m: Int) = Seq(c1, m, c2).sorted.apply(1) == m
    between(color1.getRed, color2.getRed, mid.getRed) &&
    between(color1.getGreen, color2.getGreen, mid.getGreen) &&
    between(color1.getBlue, color2.getBlue, mid.getBlue)
  }

  "RichColor string encoding" should "encode correctly to long form hex" in {
    val hex = Color.ORANGE.asHex
    hex shouldEqual "#FFC800"
    RichColor.parseOrElse(hex, Color.MAGENTA) shouldEqual Color.ORANGE
  }

  it should "encode solid color correctly to rgb" in {
    val rgb = Color.ORANGE.asRgb
    rgb shouldEqual "rgb(255,200,0)"
    RichColor.parseOrElse(rgb, Color.MAGENTA) shouldEqual Color.ORANGE
  }

  it should "encode transparent color correctly to rgb" in {
    val transparentOrange = new Color(Color.ORANGE.getRed, Color.ORANGE.getGreen, Color.ORANGE.getBlue, 127)
    val rgb = transparentOrange.asRgb
    rgb shouldEqual "rgba(255,200,0,0.5)"
    val parsed = RichColor.parseOrElse(rgb, Color.MAGENTA)
    parsed shouldEqual transparentOrange
  }

  "Color interpolation" should "generate expected value" in {
    val (from, to) = (Color.CYAN, Color.ORANGE)
    val mid = RichColor.interp(from, to, 0.5)
    def interp(a: Int, b: Int) = Math.min(a,b) + (Math.abs(a-b) * 0.5).toInt
    interp(from.getRed, to.getRed) shouldEqual mid.getRed
    interp(from.getGreen, to.getGreen)  shouldEqual mid.getGreen
    interp(from.getBlue, to.getBlue)  shouldEqual mid.getBlue
  }

  it should "generate color range correctly" in {
    val (from, to) = (Color.CYAN, Color.ORANGE)
    val range = RichColor.range(from, to, 4)
    range.head shouldEqual from
    range.last shouldEqual to
    range.length shouldEqual 6
    range.sliding(3).forall(xs => isBetween(xs(0), xs(2), xs(1)))
  }


}
