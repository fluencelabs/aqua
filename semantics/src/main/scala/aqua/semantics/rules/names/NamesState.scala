package aqua.semantics.rules.names

import aqua.model.AquaContext
import aqua.parser.lexer.{Name, Token}
import aqua.types.{ArrowType, Type}
import cats.kernel.Monoid
import cats.syntax.functor._

case class NamesState[S[_]](
  stack: List[NamesState.Frame[S]] = Nil,
  rootArrows: Map[String, ArrowType] = Map.empty,
  constants: Map[String, Type] = Map.empty[String, Type],
  opaque: Map[String, Type] = Map.empty[String, Type],
  definitions: Map[String, Name[S]] = Map.empty[String, Name[S]]
) {

  def allNames: LazyList[String] =
    LazyList
      .from(stack)
      .flatMap(s => s.names.keys ++ s.arrows.keys)
      .appendedAll(rootArrows.keys)
      .appendedAll(constants.keys)

  def allArrows: LazyList[String] =
    LazyList.from(stack).flatMap(_.arrows.keys).appendedAll(rootArrows.keys)
}

object NamesState {

  case class Frame[S[_]](
    token: Token[S],
    names: Map[String, Type] = Map.empty,
    arrows: Map[String, ArrowType] = Map.empty
  ) {
    def addName(n: String, t: Type): NamesState.Frame[S] = copy[S](names = names.updated(n, t))

    def addArrow(n: String, g: ArrowType): NamesState.Frame[S] =
      copy[S](arrows = arrows.updated(n, g))
  }

  implicit def namesStateMonoid[S[_]]: Monoid[NamesState[S]] = new Monoid[NamesState[S]] {
    override def empty: NamesState[S] = NamesState[S]()

    override def combine(x: NamesState[S], y: NamesState[S]): NamesState[S] =
      NamesState(
        stack = Nil,
        rootArrows = x.rootArrows ++ y.rootArrows,
        definitions = x.definitions ++ y.definitions,
        constants = x.constants ++ y.constants
      )
  }

  def init[S[_]](context: AquaContext): NamesState[S] =
    NamesState(
      rootArrows = context.allFuncs().map { case (s, fc) => (s, fc.arrowType) },
      constants = context.allValues().map { case (s, vm) => (s, vm.lastType) }
    )
}
