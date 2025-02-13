package io.udash.web.guide.views.ext.demo.bootstrap

import io.udash.bootstrap.button.UdashButtonOptions
import io.udash.css.CssView
import io.udash.web.guide.demos.AutoDemo
import io.udash.web.guide.styles.partials.GuideStyles
import scalatags.JsDom.all._

object PopoversDemo extends AutoDemo with CssView {

  private val (rendered, source) = {
    import io.udash._
    import io.udash.bootstrap._
    import BootstrapStyles._
    import io.udash.bootstrap.badge.UdashBadge
    import io.udash.bootstrap.button.UdashButton
    import io.udash.bootstrap.tooltip.UdashPopover
    import scalatags.JsDom.all._

    import scala.concurrent.duration.DurationInt

    val popoverContainerId = ComponentId("popover-container")
    val label1 = UdashBadge()(_ => Seq[Modifier](
      "Popover on hover with delay",
      Spacing.margin(size = SpacingSize.Small)
    )).render
    UdashPopover(
      trigger = Seq(UdashPopover.Trigger.Hover),
      delay = UdashPopover.Delay(500.millis, 250.millis),
      title = "Popover...",
      content = "Content...".render,
      container = Some(s"#$popoverContainerId")
    )(label1)

    val label2 = UdashBadge()(_ => Seq[Modifier](
      "Popover on click",
      Spacing.margin(size = SpacingSize.Small)
    )).render
    UdashPopover(
      trigger = Seq(UdashPopover.Trigger.Click),
      delay = UdashPopover.Delay(0.millis, 250.millis),
      placement = UdashPopover.Placement.Bottom,
      title = "Popover 2...",
      content = "Content...".render,
      container = Some(s"#$popoverContainerId")
    )(label2)

    val label3 = UdashBadge()(_ => Seq[Modifier](
      "Popover with JS toggler",
      Spacing.margin(size = SpacingSize.Small)
    )).render
    val label3Tooltip = UdashPopover(
      trigger = Seq(UdashPopover.Trigger.Manual),
      placement = UdashPopover.Placement.Left,
      html = true,
      title = "Popover 3...",
      content = Seq(
          p("HTML content..."),
          ul(li("Item 1"), li("Item 2"), li("Item 3"))
      ).render,
      container = Some(s"#$popoverContainerId")
    )(label3)

    val button = UdashButton()("Toggle popover")
    button.listen { case _ => label3Tooltip.toggle() }

    div(popoverContainerId)(
      label1, label2, label3, button
    ).render
  }.withSourceCode

  override protected def demoWithSource(): (Modifier, Iterator[String]) = {
    (rendered.setup(_.applyTags(GuideStyles.frame)), source.linesIterator)
  }
}

