package aqua.semantics.header

import aqua.helpers.data.PName
import aqua.helpers.data.SName
import aqua.raw.{RawContext, RawPart}
import aqua.types.{AbilityType, ArrowType, Type}

import cats.Semigroup
import cats.syntax.foldable.*
import cats.syntax.semigroup.*

// Able to pick info from different contexts
trait Picker[A] {
  def funcNames(ctx: A): Set[String]
  def definedAbilityNames(ctx: A): Set[String]
  def blank: A
  def pick(ctx: A, name: PName, rename: Option[PName]): Option[A]
  def pickDeclared(ctx: A): A
  def pickHeader(ctx: A): A
  def module(ctx: A): Option[PName]
  def allNames(ctx: A): Set[String]
  def declares(ctx: A): Set[PName]
  def exports(ctx: A): Map[String, Option[String]]
  def isAbility(ctx: A, name: String): Boolean
  def funcReturnAbilityOrArrow(ctx: A, name: String): Boolean
  def funcAcceptAbility(ctx: A, name: String): Boolean
  def setAbility(ctx: A, path: PName, ctxAb: A): A
  def setImportPaths(ctx: A, importPaths: Map[String, String]): A
  def setModule(ctx: A, name: Option[PName]): A
  def linearize(ctx: A, path: PName): A
  def setDeclares(ctx: A, declares: Set[PName]): A
  def setExports(ctx: A, exports: Map[String, Option[String]]): A
  def addPart(ctx: A, part: (A, RawPart)): A
}

object Picker {

  extension [A: Picker](p: A) {

    def blank: A = Picker[A].blank

    def funcNames: Set[String] = Picker[A].funcNames(p)
    def definedAbilityNames: Set[String] = Picker[A].definedAbilityNames(p)

    def pick(name: PName, rename: Option[PName]): Option[A] =
      Picker[A].pick(p, name, rename)

    def pickDeclared: A = Picker[A].pickDeclared(p)
    def pickHeader: A = Picker[A].pickHeader(p)
    def module: Option[PName] = Picker[A].module(p)
    def declares: Set[PName] = Picker[A].declares(p)
    def allNames: Set[String] = Picker[A].allNames(p)
    def exports: Map[String, Option[String]] = Picker[A].exports(p)

    def isAbility(name: String): Boolean = Picker[A].isAbility(p, name)

    def funcReturnAbilityOrArrow(name: String): Boolean =
      Picker[A].funcReturnAbilityOrArrow(p, name)

    def funcAcceptAbility(name: String): Boolean =
      Picker[A].funcAcceptAbility(p, name)

    def setAbility(path: PName, ctx: A): A =
      Picker[A].setAbility(p, path, ctx)

    def setImportPaths(importPaths: Map[String, String]): A =
      Picker[A].setImportPaths(p, importPaths)

    def addPart(part: (A, RawPart)): A = Picker[A].addPart(p, part)

    def addParts(parts: List[RawPart]): A =
      parts.foldLeft(p) { case (ctx, part) => ctx.addPart(ctx -> part) }

    def addFreeParts(parts: List[RawPart]): A =
      parts.foldLeft(p) { case (ctx, part) => ctx.addPart(blank -> part) }

    def setModule(name: Option[PName]): A =
      Picker[A].setModule(p, name)

    def linearize(path: PName): A =
      Picker[A].linearize(p, path)

    def setDeclares(declares: Set[PName]): A =
      Picker[A].setDeclares(p, declares)

    def setExports(exports: Map[String, Option[String]]): A =
      Picker[A].setExports(p, exports)
  }

  private def returnsAbilityOrArrow(arrowType: ArrowType): Boolean =
    arrowType.codomain.toList.exists {
      case _: AbilityType => true
      case _: ArrowType => true
      case _ => false
    }

  private def acceptsAbility(arrowType: ArrowType): Boolean =
    arrowType.domain.toList.exists {
      case _: AbilityType => true
      case _ => false
    }

  private def isAbilityType(`type`: Type): Boolean =
    `type` match {
      case _: AbilityType => true
      case _ => false
    }

  final def apply[A](using ev: Picker[A]): Picker[A] = ev

  given Picker[RawContext] with {

    override def blank: RawContext = RawContext.blank
    override def exports(ctx: RawContext): Map[String, Option[String]] = ctx.exports

    override def isAbility(ctx: RawContext, name: String): Boolean =
      ctx.types.get(SName.nameUnsafe(name)).exists(isAbilityType)

    override def funcReturnAbilityOrArrow(ctx: RawContext, name: String): Boolean =
      ctx.funcs.get(SName.nameUnsafe(name)).map(_.arrow.`type`).exists(returnsAbilityOrArrow)

    override def funcAcceptAbility(ctx: RawContext, name: String): Boolean =
      ctx.funcs.get(SName.nameUnsafe(name)).map(_.arrow.`type`).exists(acceptsAbility)

    override def funcNames(ctx: RawContext): Set[String] =
      ctx.funcs.keySet.map(_.name)

    override def definedAbilityNames(ctx: RawContext): Set[String] =
      ctx.definedAbilities.keySet.map(_.name)

    override def addPart(ctx: RawContext, part: (RawContext, RawPart)): RawContext =
      ctx.copy(parts = ctx.parts :+ part)

    override def module(ctx: RawContext): Option[PName] =
      ctx.module

    override def declares(ctx: RawContext): Set[PName] = ctx.declares

    override def allNames(ctx: RawContext): Set[String] = ctx.allNames

    override def setAbility(ctx: RawContext, path: PName, ctxAb: RawContext): RawContext =
      RawContext.abilitiesLens.modify(
        _.updated(path.head, ctxAb.linearize(path))
      )(ctx)

    // dummy
    override def setImportPaths(ctx: RawContext, importPaths: Map[String, String]): RawContext =
      ctx

    override def setModule(ctx: RawContext, name: Option[PName]): RawContext =
      ctx.copy(module = name)

    override def linearize(ctx: RawContext, path: PName): RawContext =
      ctx.linearize(path)

    override def setDeclares(ctx: RawContext, declares: Set[PName]): RawContext =
      ctx.copy(declares = declares)

    override def setExports(ctx: RawContext, exports: Map[String, Option[String]]): RawContext =
      ctx.copy(exports = exports)

    private def search(
      ctx: RawContext,
      name: PName
    ): Option[RawContext | RawContext.Parts] =
      name.uncons match {
        case (sname, Some(path)) =>
          ctx.abilities
            .get(sname)
            .flatMap { ab =>
              search(ab, path)
            }
        case (sname, None) =>
          ctx.abilities.get(sname) orElse {
            val parts = ctx.parts.filter { case (partContext, part) =>
              part.name == sname.name
            }

            Option.when(parts.nonEmpty)(parts)
          }
      }

    override def pick(
      ctx: RawContext,
      name: PName,
      rename: Option[PName]
    ): Option[RawContext] =
      search(ctx, name).map { result =>
        val newName = rename.getOrElse(name)
        val (path, innerName) = newName.unconsR
        val inner = result match {
          case ability: RawContext =>
            RawContext.fromAbilities(
              Map(innerName -> ability)
            )
          case parts: RawContext.Parts =>
            RawContext.fromParts(
              parts.map { case (partContext, part) =>
                (partContext, part.rename(innerName.name))
              }
            )
        }

        path.fold(inner)(subPath => blank.setAbility(subPath, inner))
      }

    override def pickHeader(ctx: RawContext): RawContext =
      RawContext.blank.copy(module = ctx.module, declares = ctx.declares, exports = ctx.exports)

    override def pickDeclared(ctx: RawContext): RawContext =
      if (ctx.module.isEmpty) ctx
      else
        ctx.declares.toList
          .flatMap(n => pick(ctx, n, rename = None))
          .foldLeft(pickHeader(ctx))(_ |+| _)
  }

}
