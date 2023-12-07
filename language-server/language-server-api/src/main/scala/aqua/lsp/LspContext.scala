package aqua.lsp

import aqua.parser.lexer.{LiteralToken, NamedTypeToken, Token}
import aqua.raw.{RawContext, RawPart}
import aqua.semantics.header.Picker
import aqua.semantics.rules.locations.{TokenLocation, VariableInfo}
import aqua.semantics.{SemanticError, SemanticWarning}
import aqua.types.{AbilityType, ArrowType, Type}
import cats.syntax.monoid.*
import cats.{Monoid, Semigroup}

// Context with info that necessary for language server
case class LspContext[S[_]](
  raw: RawContext,
  abDefinitions: Map[String, NamedTypeToken[S]] = Map.empty[String, NamedTypeToken[S]],
  rootArrows: Map[String, ArrowType] = Map.empty[String, ArrowType],
  constants: Map[String, Type] = Map.empty[String, Type],
  variables: List[VariableInfo[S]] = Nil,
  importTokens: List[LiteralToken[S]] = Nil,
  errors: List[SemanticError[S]] = Nil,
  warnings: List[SemanticWarning[S]] = Nil
) {
  lazy val allLocations: List[TokenLocation[S]] = variables.flatMap(_.allLocations)
  // there can be duplicates of variables with the same token
  lazy val allVariablesMerged: List[VariableInfo[S]] = variables.foldLeft(Map.empty[Token[S], VariableInfo[S]]) {
    case (acc, vi) =>
      acc.updated(vi.definition.token, acc.get(vi.definition.token).map { v =>
        v.copy(occurrences = v.occurrences ++ vi.occurrences)
      }.getOrElse(vi))
  }.toList.map(_._2)
}

object LspContext {

  def blank[S[_]]: LspContext[S] = LspContext[S](raw = RawContext())

  given [S[_]]: Semigroup[LspContext[S]] =
    (x: LspContext[S], y: LspContext[S]) =>
      LspContext[S](
        raw = x.raw |+| y.raw,
        abDefinitions = x.abDefinitions ++ y.abDefinitions,
        rootArrows = x.rootArrows ++ y.rootArrows,
        constants = x.constants ++ y.constants,
        importTokens = x.importTokens ++ y.importTokens,
        variables = x.variables ++ y.variables,
        errors = x.errors ++ y.errors,
        warnings = x.warnings ++ y.warnings
      )

  trait Implicits[S[_]] {
    val lspContextMonoid: Monoid[LspContext[S]]
  }

  def implicits[S[_]](init: LspContext[S]): Implicits[S] = new Implicits[S] {

    override val lspContextMonoid: Monoid[LspContext[S]] = new Monoid[LspContext[S]] {
      override def empty: LspContext[S] = init

      override def combine(x: LspContext[S], y: LspContext[S]): LspContext[S] = {
        Semigroup[LspContext[S]].combine(x, y)
      }
    }

  }

  given [S[_]]: Picker[LspContext[S]] with {
    import aqua.semantics.header.Picker.*

    override def blank: LspContext[S] = LspContext[S](Picker[RawContext].blank, Map.empty)
    override def exports(ctx: LspContext[S]): Map[String, Option[String]] = ctx.raw.exports

    override def isAbility(ctx: LspContext[S], name: String): Boolean =
      ctx.raw.isAbility(name)

    override def funcReturnAbilityOrArrow(ctx: LspContext[S], name: String): Boolean =
      ctx.raw.funcReturnAbilityOrArrow(name)

    override def funcAcceptAbility(ctx: LspContext[S], name: String): Boolean =
      ctx.raw.funcAcceptAbility(name)

    override def funcNames(ctx: LspContext[S]): Set[String] = ctx.raw.funcNames

    override def definedAbilityNames(ctx: LspContext[S]): Set[String] =
      ctx.raw.definedAbilityNames

    override def addPart(ctx: LspContext[S], part: (LspContext[S], RawPart)): LspContext[S] =
      ctx.copy(raw = ctx.raw.addPart(part._1.raw -> part._2))

    override def setInit(ctx: LspContext[S], ctxInit: Option[LspContext[S]]): LspContext[S] =
      ctx.copy(raw = ctx.raw.setInit(ctxInit.map(_.raw)))

    override def all(ctx: LspContext[S]): Set[String] =
      ctx.raw.all
    override def module(ctx: LspContext[S]): Option[String] = ctx.raw.module
    override def declares(ctx: LspContext[S]): Set[String] = ctx.raw.declares

    override def setAbility(ctx: LspContext[S], name: String, ctxAb: LspContext[S]): LspContext[S] =
      ctx.copy(
        raw = ctx.raw.setAbility(name, ctxAb.raw),
        variables = ctx.variables ++ ctxAb.variables.map(v =>
          v.copy(definition =
            v.definition.copy(name = AbilityType.fullName(name, v.definition.name))
          )
        )
      )

    override def setModule(
      ctx: LspContext[S],
      name: Option[String],
      declares: Set[String]
    ): LspContext[S] =
      ctx.copy(raw = ctx.raw.setOptModule(name, declares))

    override def setExports(
      ctx: LspContext[S],
      exports: Map[String, Option[String]]
    ): LspContext[S] =
      ctx.copy(raw = ctx.raw.setExports(exports))

    override def pick(
      ctx: LspContext[S],
      name: String,
      rename: Option[String],
      declared: Boolean
    ): Option[LspContext[S]] =
      // rename tokens from one context with prefix addition
      val newVariables = rename.map { renameStr =>
        ctx.variables.map {
          case v if v.definition.name.startsWith(name) =>
            v.copy(definition =
              v.definition.copy(name = v.definition.name.replaceFirst(v.definition.name, renameStr))
            )

          case kv => kv
        }
      }.getOrElse(ctx.variables)

      ctx.raw
        .pick(name, rename, declared)
        .map(rc =>
          ctx.copy(
            raw = rc,
            abDefinitions =
              ctx.abDefinitions.get(name).fold(Map.empty)(t => Map(rename.getOrElse(name) -> t)),
            rootArrows =
              ctx.rootArrows.get(name).fold(Map.empty)(t => Map(rename.getOrElse(name) -> t)),
            constants =
              ctx.constants.get(name).fold(Map.empty)(t => Map(rename.getOrElse(name) -> t)),
            variables = newVariables
          )
        )

    override def pickHeader(ctx: LspContext[S]): LspContext[S] = ctx.copy(raw = ctx.raw.pickHeader)

    override def pickDeclared(
      ctx: LspContext[S]
    )(using Semigroup[LspContext[S]]): LspContext[S] =
      ctx.copy(raw = ctx.raw.pickDeclared)
  }
}
