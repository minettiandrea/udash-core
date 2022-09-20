package io.udash.bindings.inputs

import io.udash._
import org.scalajs.dom.{Event, window}
import org.scalajs.dom.html.Select
import scalatags.JsDom.all._

import scala.concurrent.duration.Duration

private[inputs] class SelectBinding[T](
  options: ReadableSeqProperty[T], label: T => Modifier, labelNoValue: Option[Modifier], debounce: Option[Duration], selectModifiers: Modifier*
)(
  checkedIf: T => ReadableProperty[Boolean],
  refreshSelection: Seq[T] => Unit,
  onChange: Select => Event => Unit
) extends InputBinding[Select] {
  private val selector = select(selectModifiers)(
    produce(options) { opts =>
      kill()
      refreshSelection(opts)

      val empty = labelNoValue.map { l =>
        option(l, value := "").render
      }

      {
          empty.iterator ++ opts.iterator.zipWithIndex.map { case (opt, idx) =>
            val el = option(value := idx.toString, label(opt)).render

            val selected = checkedIf(opt)
            propertyListeners += selected.listen(el.selected = _, initUpdate = true)
            el
          }
      }.toSeq
    }
  ).render

  var propertyUpdateHandler: Int = 0
  val callback = if (debounce.nonEmpty && debounce.get.toMillis > 0) {
    e: Event => {
      if (propertyUpdateHandler != 0) window.clearTimeout(propertyUpdateHandler)
      propertyUpdateHandler = window.setTimeout(() => {
        onChange(selector)(e)
      }, debounce.get.toMillis.toDouble)
    }
  } else {
    e: Event => onChange(selector)(e)
  }

  selector.onchange = callback

  override def render: Select = selector
}