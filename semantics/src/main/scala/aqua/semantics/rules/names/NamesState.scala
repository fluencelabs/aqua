package aqua.semantics.rules.names

import aqua.parser.lexer.{Name, Token}
import aqua.types.{ArrowType, Type}
import cats.kernel.Monoid

// TODO: use models if possible
case class NamesState[F[_]](
  stack: List[NamesState.Frame[F]] = Nil,
  // resolved functions
  rootArrows: Map[String, ArrowType] = Map.empty,
  // resolved constants
  constants: Map[String, Type] = Map.empty[String, Type],
  // TODO: check if it could be a function
  definitions: Map[String, Name[F]] = Map.empty[String, Name[F]]
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

  // def apply(usedModels, context)

  case class Frame[F[_]](
    token: Token[F],
    names: Map[String, Type] = Map.empty,
    arrows: Map[String, ArrowType] = Map.empty
  ) {
    def addName(n: String, t: Type): NamesState.Frame[F] = copy[F](names = names.updated(n, t))

    def addArrow(n: String, g: ArrowType): NamesState.Frame[F] =
      copy[F](arrows = arrows.updated(n, g))
  }

  implicit def namesStateMonoid[F[_]]: Monoid[NamesState[F]] = new Monoid[NamesState[F]] {
    override def empty: NamesState[F] = NamesState[F]()

    override def combine(x: NamesState[F], y: NamesState[F]): NamesState[F] =
      NamesState(
        stack = Nil,
        rootArrows = x.rootArrows ++ y.rootArrows,
        definitions = x.definitions ++ y.definitions,
        constants = x.constants ++ y.constants
      )
  }
}
