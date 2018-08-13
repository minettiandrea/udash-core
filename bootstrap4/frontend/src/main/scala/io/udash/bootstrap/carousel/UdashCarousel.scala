package io.udash.bootstrap
package carousel

import com.avsystem.commons.misc._
import io.udash._
import io.udash.bindings.modifiers.Binding
import io.udash.bootstrap.carousel.UdashCarousel.AnimationOptions.PauseOption
import io.udash.bootstrap.carousel.UdashCarousel.CarouselEvent.Direction
import io.udash.bootstrap.carousel.UdashCarousel.{AnimationOptions, CarouselEvent}
import io.udash.properties.seq
import io.udash.wrappers.jquery.JQuery
import org.scalajs.dom
import org.scalajs.dom.{Element, Node}
import scalatags.JsDom.all._

import scala.scalajs.js
import scala.scalajs.js.Dictionary
import scala.util.Try

final class UdashCarousel[ItemType, ElemType <: ReadableProperty[ItemType]] private(
  slides: seq.ReadableSeqProperty[ItemType, ElemType],
  showIndicators: ReadableProperty[Boolean],
  animationOptions: ReadableProperty[AnimationOptions],
  val activeSlide: Property[Int],
  val componentId: ComponentId,
)(
  slideContentFactory: (ElemType, Binding.NestedInterceptor) => Modifier
) extends UdashBootstrapComponent with Listenable[UdashCarousel[ItemType, ElemType], CarouselEvent[ItemType, ElemType]] {

  import BootstrapStyles.Carousel
  import BootstrapTags._
  import UdashCarousel._
  import io.udash.css.CssView._
  import io.udash.wrappers.jquery._

  if (activeSlide.get >= slides.size) activeSlide.set(slides.size - 1)
  if (activeSlide.get < 0) activeSlide.set(0)

  propertyListeners += slides.listenStructure { patch =>
    val active = activeSlide.get
    if (patch.idx <= active) {
      if (patch.idx + patch.removed.size <= active) {
        activeSlide.set(active - patch.removed.size + patch.added.size)
      } else {
        activeSlide.set(if (patch.idx != 0) patch.idx - 1 else 0)
      }
    }
  }

  override val render: Element = {
    def indicators(): Binding = {
      def indicator(index: Int) = li(
        dataTarget := s"#$componentId", dataSlideTo := index,
        BootstrapStyles.active.styleIf(activeSlide.transform(_ == index))
      )

      val indices = slides.transform((slides: Seq[_]) => slides.length)
      produce(indices) { length =>
        ol(Carousel.indicators)(
          (0 until length).map(indicator).render
        ).render
      }
    }

    val slidesComponent = div(Carousel.inner, role := "listbox")(
      nestedInterceptor(repeatWithIndex(slides) { (slide, idx, nested) =>
        div(
          BootstrapStyles.Carousel.item,
          BootstrapStyles.active.styleIf(idx.combine(activeSlide)(_ == _))
        )(
          slideContentFactory(slide, nested)
        ).render
      })
    ).render

    def extractEventData(ev: JQueryEvent): (Int, Direction) = {
      val idx = jQ(slidesComponent).children().index(ev.relatedTarget)
      val direction = Try {
        val directionString = ev.asInstanceOf[Dictionary[String]].apply("direction")

        if (directionString == "left") CarouselEvent.Direction.Left
        else if (directionString == "right") CarouselEvent.Direction.Right
        else CarouselEvent.Direction.Unknown
      }
      (idx, direction.getOrElse(CarouselEvent.Direction.Unknown))
    }

    val res: Element = div(id := componentId, Carousel.carousel, Carousel.slide)(
      nestedInterceptor(produceWithNested(showIndicators) {
        case (true, nested) => span(nested(indicators())).render
        case (false, _) => span().render
      }),
      slidesComponent,
      a(Carousel.controlPrev, Carousel.control, href := s"#$componentId", role := "button", dataSlide := "prev")(
        span(Carousel.controlPrevIcon),
        span(cls := "sr-only", "Previous")
      ),
      a(Carousel.controlNext, Carousel.control, href := s"#$componentId", role := "button", dataSlide := "next")(
        span(Carousel.controlNextIcon),
        span(cls := "sr-only", "Next")
      )
    ).render

    val jqCarousel = jQ(res).asInstanceOf[UdashCarouselJQuery]
    jqCarousel.on("slide.bs.carousel", (_: dom.Element, ev: JQueryEvent) => {
      val (idx, dir) = extractEventData(ev)
      activeSlide.set(idx)
      fire(SlideChangeEvent(this, idx, dir))
    })
    jqCarousel.on("slid.bs.carousel", (_: dom.Element, ev: JQueryEvent) => {
      val (idx, dir) = extractEventData(ev)
      activeSlide.set(idx)
      fire(SlideChangedEvent(this, idx, dir))
    })

    propertyListeners += animationOptions.listen { animationOptions =>
      jqCarousel.carousel(animationOptions.native)
      if (!animationOptions.active) jqCarousel.carousel("pause")
    }

    res
  }

  /** Turn on slide transition. */
  def cycle(): Unit = jQSelector().carousel("cycle")

  /** Pause slide transition. */
  def pause(): Unit = jQSelector().carousel("pause")

  /**
    * Change active slide.
    *
    * @param slideNumber new active slide index
    */
  def goTo(slideNumber: Int): Unit = jQSelector().carousel(slideNumber)

  /** Change active slide to the next one (index order). */
  def nextSlide(): Unit = jQSelector().carousel("next")

  /** Change active slide to the previous one (index order). */
  def previousSlide(): Unit = jQSelector().carousel("prev")

  private def jQSelector(): UdashCarouselJQuery = jQ(render).asInstanceOf[UdashCarouselJQuery]
}

object UdashCarousel {
  /**
    * Creates the UdashCarousel component.
    * More: <a href="http://getbootstrap.com/javascript/#carousel">Bootstrap Docs</a>.
    *
    * @param slides              SeqProperty of carousel slides.
    * @param showIndicators      Show carousel slide indicators.
    * @param animationOptions    Carousel animation options.
    * @param activeSlide         Active carousel slide.
    * @param componentId         Carousel div ID.
    * @param slideContentFactory Creates content of a slide.
    * @return `UdashCarousel` component
    */
  def apply[ItemType, ElemType <: ReadableProperty[ItemType]](
    slides: seq.ReadableSeqProperty[ItemType, ElemType],
    showIndicators: ReadableProperty[Boolean] = UdashBootstrap.True,
    animationOptions: ReadableProperty[AnimationOptions] = AnimationOptions().toProperty,
    activeSlide: Property[Int] = Property(0),
    componentId: ComponentId = ComponentId.newId()
  )(
    slideContentFactory: (ElemType, Binding.NestedInterceptor) => Modifier
  ): UdashCarousel[ItemType, ElemType] = {
    new UdashCarousel(slides, showIndicators, animationOptions, activeSlide, componentId)(slideContentFactory)
  }

  /**
    * Creates the UdashCarousel component consisting of `UdashCarouselSlide`.
    * More: <a href="http://getbootstrap.com/javascript/#carousel">Bootstrap Docs</a>.
    *
    * @param slides           SeqProperty of carousel slides.
    * @param showIndicators   Show carousel slide indicators.
    * @param animationOptions Carousel animation options.
    * @param activeSlide      Active carousel slide.
    * @param componentId      Carousel div ID.
    * @param slideContentFactory Creates content of a slide.
    * @return `UdashCarousel` component
    */
  def default(
    slides: ReadableSeqProperty[UdashCarouselSlide],
    showIndicators: ReadableProperty[Boolean] = UdashBootstrap.True,
    animationOptions: ReadableProperty[AnimationOptions] = AnimationOptions().toProperty,
    activeSlide: Property[Int] = Property(0),
    componentId: ComponentId = ComponentId.newId()
  )(
    slideContentFactory: (ReadableProperty[UdashCarouselSlide], Binding.NestedInterceptor) => Modifier =
      (slide, nested) => nested(produce(slide)(_.render))
  ): UdashCarousel[UdashCarouselSlide, ReadableProperty[UdashCarouselSlide]] = {
    new UdashCarousel(slides, showIndicators, animationOptions, activeSlide, componentId)(slideContentFactory)
  }

  /** Event hierarchy for [[UdashCarousel]]-emitted events. */
  sealed trait CarouselEvent[ItemType, ElemType <: ReadableProperty[ItemType]]
    extends AbstractCase with ListenableEvent[UdashCarousel[ItemType, ElemType]] {
    /** The index of the slide source transitioned to. Either [[CarouselEvent.Direction.Left]] or [[CarouselEvent.Direction.Right]]. */
    def targetIndex: Int

    /** The animation direction. */
    def direction: Direction
  }

  /**
    * Event emitted by [[UdashCarousel]] on slide change transition start
    *
    * @param source      The [[UdashCarousel]] emitting the event.
    * @param targetIndex The index of the slide source transitioned to.
    * @param direction   The animation direction. Either [[CarouselEvent.Direction.Left]] or [[CarouselEvent.Direction.Right]].
    */
  final case class SlideChangeEvent[ItemType, ElemType <: ReadableProperty[ItemType]](
    source: UdashCarousel[ItemType, ElemType], targetIndex: Int, direction: Direction
  ) extends CarouselEvent[ItemType, ElemType]

  /**
    * Event emitted by [[UdashCarousel]] on slide change transition finish.
    *
    * @param source      The [[UdashCarousel]] emitting the event.
    * @param targetIndex The index of the slide source transitioned to.
    * @param direction   The animation direction. Either [[CarouselEvent.Direction.Left]] or [[CarouselEvent.Direction.Right]].
    */
  final case class SlideChangedEvent[ItemType, ElemType <: ReadableProperty[ItemType]](
    source: UdashCarousel[ItemType, ElemType], targetIndex: Int, direction: Direction
  ) extends CarouselEvent[ItemType, ElemType]

  object CarouselEvent {
    /** Carousel animation direction. */
    final class Direction(implicit enumCtx: EnumCtx) extends AbstractValueEnum
    object Direction extends ValueEnumCompanion[Direction] {
      final val Left, Right: Value = new Direction
      /** Animation direction from carousel.js that neither left nor right. */
      final val Unknown: Value = new Direction
    }
  }

  @js.native
  private trait UdashCarouselJQuery extends JQuery {
    def carousel(options: CarouselOptionsJS): UdashCarouselJQuery = js.native
    def carousel(cmd: String): UdashCarouselJQuery = js.native
    def carousel(number: Int): UdashCarouselJQuery = js.native
  }

  import scala.concurrent.duration._

  @js.native
  private trait CarouselOptionsJS extends js.Object {
    var interval: Int = js.native
    var pause: String = js.native
    var wrap: Boolean = js.native
    var keyboard: Boolean = js.native
  }

  /**
    * [[UdashCarousel]] animation options.
    *
    * @param interval The amount of time to delay between automatically cycling an item.
    * @param pause    Indicated whether the carousel should pass on some specific event. See [[UdashCarousel.AnimationOptions.PauseOption]].
    * @param wrap     Should the carousel cycle continuously or have hard stops.
    * @param keyboard Should the carousel react to keyboard events.
    * @param active   Should the animation be active.
    */
  case class AnimationOptions(
    interval: Duration = 5 seconds, pause: PauseOption = PauseOption.Hover, wrap: Boolean = true,
    keyboard: Boolean = true, active: Boolean = true
  ) {
    private[UdashCarousel] def native: CarouselOptionsJS = {
      val options = js.Object().asInstanceOf[CarouselOptionsJS]
      options.interval = interval.toMillis.toInt
      options.pause = pause.name
      options.wrap = wrap
      options.keyboard = keyboard
      options
    }
  }

  object AnimationOptions {
    final class PauseOption(name: String)(implicit enumCtx: EnumCtx) extends AbstractValueEnum

    object PauseOption extends AbstractValueEnumCompanion[PauseOption] {
      /** Pauses the cycling of the carousel on mouseenter and resumes the cycling of the carousel on mouseleave. */
      final val Hover: Value = new PauseOption("hover")
      final val False: Value = new PauseOption("false")
    }
  }
}

/**
  * [[UdashCarousel]] slide.
  *
  * @param imgSrc  Slide image source url.
  * @param caption Slide caption content.
  */
case class UdashCarouselSlide(imgSrc: Url)(caption: Modifier*) {
  import io.udash.css.CssView._

  lazy val render: Node = {
    Seq(
      img(src := imgSrc.value),
      div(BootstrapStyles.Carousel.caption)(
        caption
      )
    ).render
  }
}