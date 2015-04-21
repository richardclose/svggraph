import org.scalatest.{Matchers, FlatSpec}

import org.phasanix.svggraph.Helper._

/**
 *
 */
class HelperTest extends FlatSpec with Matchers {

  "Helper formatting" should "correctly encode float" in {
    val sb = new StringBuilder()
    sb.append("[").fmt(123.45, 3).append("]")
    sb.toString shouldEqual "[123.450]"

    sb.clear()
    sb.append('[').fmt(123.456789, 5)
    sb.toString shouldEqual "[123.45678"
  }

  it should "be faster than NumberFormat" in {

    val sb = new StringBuilder()
    val numFmt = java.text.NumberFormat.getInstance()
    numFmt.setMinimumFractionDigits(3)
    numFmt.setMaximumFractionDigits(3)
    numFmt.setGroupingUsed(false)

    val count = 500000
    var i: Int = 0
    var start: Long = 0L

    // warm up loop for Helper and NumberFormat
    i = 10000
    while (i > 0) {
      sb.append(numFmt.format(123.45))
      sb.fmt(123.45, 3)
      sb.setLength(0)
      i -= 1
    }

    // Measure Helper.fmt
    i = count
    start = System.nanoTime()
    while (i > 0) {
      sb.fmt(123.45, 3)
      sb.setLength(0)
      i -=  1
    }

    val t1 = (System.nanoTime().toDouble - start)/count

    // Measure NumberFormat
    i = count
    start = System.nanoTime()
    while (i > 0) {
      sb.append(numFmt.format(123.45))
      sb.setLength(0)
      i -= 1
    }

    val t2 = (System.nanoTime().toDouble - start)/count

    println(s"call to Helper.append(): $t1 ns call to NumberFormat.format(): $t2")

    (t2 - t1 > 0) shouldBe true
  }

}
