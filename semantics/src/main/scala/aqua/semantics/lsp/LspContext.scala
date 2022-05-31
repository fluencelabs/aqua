package aqua.semantics.lsp

import aqua.parser.lexer.{Ability, Name, Token}
import aqua.raw.RawContext
import aqua.types.ArrowType
import cats.{Monoid, Semigroup}
import cats.syntax.monoid.*
import RawContext.semiRC

case class LspContext[S[_]](
  raw: RawContext,
  abDefinitions: Map[String, (Ability[S], List[(Name[S], ArrowType)])] =
    Map.empty[String, (Ability[S], List[(Name[S], ArrowType)])],
  rootArrows: Map[String, TokenArrowInfo[S]] = Map.empty[String, TokenArrowInfo[S]],
  constants: Map[String, TokenType[S]] = Map.empty[String, TokenType[S]],
  locations: List[(Token[S], TokenInfo[S])] = Nil
)

object LspContext {

  def blank[S[_]]: LspContext[S] = LspContext[S](raw = RawContext())

  implicit def semiLsp[S[_]]: Semigroup[LspContext[S]] =
    (x: LspContext[S], y: LspContext[S]) =>
      LspContext[S](
        raw = x.raw |+| y.raw,
        abDefinitions = x.abDefinitions ++ y.abDefinitions,
        rootArrows = x.rootArrows ++ y.rootArrows,
        constants = x.constants ++ y.constants,
        locations = x.locations ++ y.locations
      )

  trait Implicits[S[_]] {
    implicit val lspContextMonoid: Monoid[LspContext[S]]
  }

  def implicits[S[_]](init: LspContext[S]): Implicits[S] = new Implicits[S] {

    override implicit val lspContextMonoid: Monoid[LspContext[S]] = new Monoid[LspContext[S]] {
      override def empty: LspContext[S] = init

      override def combine(x: LspContext[S], y: LspContext[S]): LspContext[S] = {
        semiLsp[S].combine(x, y)
      }
    }

  }
}
