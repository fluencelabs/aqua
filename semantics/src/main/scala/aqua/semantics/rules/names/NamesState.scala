package aqua.semantics.rules.names

import aqua.parser.lexer.{Name, Token}
import aqua.raw.RawContext
import aqua.types.{ArrowType, Type}
import cats.kernel.Monoid
import cats.syntax.functor.*

sealed trait TokenInfo[F[_]] {
  def definition: Option[Token[F]]
  def tokenType: Type
}
case class TokenTypeInfo[F[_]](definition: Option[Token[F]], tokenType: Type) extends TokenInfo[F]

case class TokenArrowInfo[F[_]](definition: Option[Token[F]], tokenType: ArrowType)
    extends TokenInfo[F]

case class NamesState[S[_]](
  stack: List[NamesState.Frame[S]] = Nil,
  rootArrows: Map[String, TokenArrowInfo[S]] = Map.empty[String, TokenArrowInfo[S]],
  constants: Map[String, TokenInfo[S]] = Map.empty[String, TokenInfo[S]],
  definitions: Map[String, Name[S]] = Map.empty[String, Name[S]],
  locations: List[(Token[S], TokenInfo[S])] = Nil
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
    names: Map[String, TokenInfo[S]] = Map.empty[String, TokenInfo[S]],
    arrows: Map[String, TokenArrowInfo[S]] = Map.empty[String, TokenArrowInfo[S]]
  ) {

    def addName(n: Name[S], t: Type): NamesState.Frame[S] =
      copy[S](names = names.updated(n.value, TokenTypeInfo(Some(n), t)))

    def addArrow(n: Name[S], g: ArrowType): NamesState.Frame[S] =
      copy[S](arrows = arrows.updated(n.value, TokenArrowInfo(Some(n), g)))
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

  def init[S[_]](context: RawContext): NamesState[S] =
    NamesState(
      rootArrows = context.allFuncs.map { case (s, fc) =>
        (s, TokenArrowInfo[S](None, fc.arrow.`type`))
      },
      constants = context.allValues.map { case (s, vm) => (s, TokenTypeInfo[S](None, vm.`type`)) }
    )
}
