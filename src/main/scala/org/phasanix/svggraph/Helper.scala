package org.phasanix.svggraph

/**
 * helper methods
 */
object Helper {

  /**
   * Fast append double value to string buffer with fixed dp
   * Thread-safe alternative to java.text.NumberFormat
   */
  def append(sb: StringBuilder, value: Double, dp: Int): Unit = {
    val insertAt = sb.length

    val absV = if (value < 0) {
      sb.append('-')
      -value
    } else value

    var intV = absV.toInt

    while (intV > 0) {
      sb.insert(insertAt, ((intV % 10) + '0').toChar)
      intV /= 10
    }

    if (dp > 0) {
      val pow = dp match {
        case 1 => 10
        case 2 => 100
        case 3 => 1000
        case 4 => 10000
        case _ =>
          var count = dp
          var p = 1
          while (count > 0) {
            p *= 10
            count -= 1
          }
          p
      }

      var decV = ((absV % 1) * pow).toInt
      sb.append('.')
      val insertDp = sb.length
      var count = dp
      while (count > 0) {
        sb.insert(insertDp, ((decV % 10) + '0').toChar)
        decV /= 10
        count -= 1
      }
    }
  }

  implicit class StringBuilderOps(val sb: StringBuilder) extends AnyVal {
    def fmt(value: Double, dp: Int): StringBuilder = {
      append(sb, value, dp)
      sb
    }
  }

}
