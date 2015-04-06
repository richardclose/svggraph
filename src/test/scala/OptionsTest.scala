import org.phasanix.svggraph.Options
import org.scalatest.{Matchers, FlatSpec}

/**
 */

class OptionsTest extends FlatSpec with Matchers {

  "Options.Layout" should "have correct total dimensions" in {

    val layout = Options.Layout.basic(chartWidth = 500, chartHeight =  300, xTickMargin = 44, yTickMarginLeft = 55)

    val w = layout.yTickAreaLeft.width + (2*layout.plotMargin) + layout.plotArea.width + layout.yTickMarginRight
    val h = layout.xTickArea.height + layout.plotMargin + layout.plotArea.height

    w shouldEqual layout.chartArea.width
    h shouldEqual layout.chartArea.height
  }

  it should "position plot area correctly" in {
    val layout = Options.Layout.basic(chartWidth = 500, chartHeight =  300, xTickMargin = 44, yTickMarginLeft = 55)

    layout.chartArea.height shouldEqual layout.plotArea.y + layout.plotArea.height
    layout.chartArea.width shouldEqual layout.yTickAreaRight.x + layout.yTickAreaRight.width
  }

}
