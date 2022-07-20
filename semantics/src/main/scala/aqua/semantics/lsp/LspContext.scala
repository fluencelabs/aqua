package aqua.semantics.lsp

import aqua.parser.lexer.{Ability, LiteralToken, Name, Token}
import aqua.raw.{RawContext, RawPart}
import aqua.types.ArrowType
import cats.{Monoid, Semigroup}
import cats.syntax.monoid.*
import RawContext.semiRC
import aqua.semantics.header.{Picker, PickerOps}

// Context with info that necessary for language server
case class LspContext[S[_]](
  raw: RawContext,
  abDefinitions: Map[String, (Ability[S], List[(Name[S], ArrowType)])] =
    Map.empty[String, (Ability[S], List[(Name[S], ArrowType)])],
  rootArrows: Map[String, TokenArrowInfo[S]] = Map.empty[String, TokenArrowInfo[S]],
  constants: Map[String, TokenType[S]] = Map.empty[String, TokenType[S]],
  locations: List[(Token[S], TokenInfo[S])] = Nil,
  importTokens: List[LiteralToken[S]] = Nil
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

  given [S[_]]: Picker[LspContext[S]] with {

    private def ops[S[_]](ctx: LspContext[S]) = PickerOps[RawContext](ctx.raw)

    override def blank: LspContext[S] = LspContext[S](Picker[RawContext].blank, Map.empty)
    override def exports(ctx: LspContext[S]): Option[Map[String, Option[String]]] = ops(ctx).exports
    override def funcNames(ctx: LspContext[S]): List[String] = ops(ctx).funcNames

    override def addPart(ctx: LspContext[S], part: (LspContext[S], RawPart)): LspContext[S] =
      ctx.copy(raw = ops(ctx).addPart(part._1.raw -> part._2))

    override def setInit(ctx: LspContext[S], ctxInit: Option[LspContext[S]]): LspContext[S] =
      ctx.copy(raw = ops(ctx).setInit(ctxInit.map(_.raw)))

    override def all(ctx: LspContext[S]): Set[String] =
      ops(ctx).all
    override def module(ctx: LspContext[S]): Option[String] = ops(ctx).module
    override def declares(ctx: LspContext[S]): Set[String] = ops(ctx).declares

    override def setAbility(ctx: LspContext[S], name: String, ctxAb: LspContext[S]): LspContext[S] =
      ctx.copy(raw = ops(ctx).setAbility(name, ctxAb.raw))

    override def setModule(
      ctx: LspContext[S],
      name: Option[String],
      declares: Set[String]
    ): LspContext[S] =
      ctx.copy(raw = ops(ctx).setOptModule(name, declares))

    override def setExports(
      ctx: LspContext[S],
      exports: Map[String, Option[String]]
    ): LspContext[S] =
      ctx.copy(raw = ops(ctx).setExports(exports))

    override def pick(
      ctx: LspContext[S],
      name: String,
      rename: Option[String],
      declared: Boolean
    ): Option[LspContext[S]] =
      ops(ctx)
        .pick(name, rename, declared)
        .map(rc =>
          ctx.copy(
            raw = rc,
            abDefinitions =
              ctx.abDefinitions.get(name).fold(Map.empty)(t => Map(rename.getOrElse(name) -> t)),
            rootArrows =
              ctx.rootArrows.get(name).fold(Map.empty)(t => Map(rename.getOrElse(name) -> t)),
            constants =
              ctx.constants.get(name).fold(Map.empty)(t => Map(rename.getOrElse(name) -> t))
          )
        )

    override def pickHeader(ctx: LspContext[S]): LspContext[S] =
      ctx.copy(raw = ops(ctx).pickHeader)

    override def pickDeclared(
      ctx: LspContext[S]
    )(implicit semi: Semigroup[LspContext[S]]): LspContext[S] =
      ctx.copy(raw = ops(ctx).pickDeclared)
  }
}
