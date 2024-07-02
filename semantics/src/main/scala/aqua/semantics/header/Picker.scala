/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.semantics.header

import aqua.helpers.data.{PName, SName}
import aqua.raw.{RawContext, RawPart}
import aqua.types.{AbilityType, ArrowType, StreamMapType, Type}

import cats.Semigroup
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.semigroup.*

/**
 * Typeclass of compilation contexts.
 * Refer to `Picker.Laws` for laws of this typeclass.
 */
trait Picker[A] {
  def funcNames(ctx: A): Set[String]
  def definedAbilityNames(ctx: A): Set[String]
  def blank: A
  def pick(ctx: A, name: PName, rename: Option[PName]): Option[A]
  def pickDeclared(ctx: A): A
  def pickHeader(ctx: A): A

  def moduleName(ctx: A): Option[PName]
  def declares(ctx: A): Set[PName]
  def exports(ctx: A): Map[PName, Option[PName]]

  def allNames(ctx: A): Set[String]
  def isAbility(ctx: A, name: String): Boolean
  def funcReturnAbilityOrArrow(ctx: A, name: String): Boolean
  def funcAcceptAbility(ctx: A, name: String): Boolean
  def setImportPaths(ctx: A, importPaths: Map[String, String]): A

  /**
   * Generate new context that contains given
   * context nested by given path.
   *
   * Note: this method clears module information.
   *
   * @param ctx context to nest
   * @param path path to nested context
   * @return new context
   */
  def scoped(ctx: A, path: PName): A

  /**
   * Retrieve context contained within
   * given context by given path.
   *
   * This method behave like `pick` for contexts but
   * unnests the result, while `pick` does not.
   *
   * @param ctx context to search in
   * @param path path to search for
   * @return nested context if any
   */
  def unscoped(ctx: A, path: PName): Option[A]

  def setModuleName(ctx: A, name: PName): A
  def setDeclares(ctx: A, declares: Set[PName]): A
  def setExports(ctx: A, exports: Map[PName, Option[PName]]): A

  def clearModule(ctx: A): A

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
    def moduleName: Option[PName] = Picker[A].moduleName(p)
    def declares: Set[PName] = Picker[A].declares(p)
    def exports: Map[PName, Option[PName]] = Picker[A].exports(p)
    def allNames: Set[String] = Picker[A].allNames(p)

    def isAbility(name: String): Boolean = Picker[A].isAbility(p, name)

    def funcReturnAbilityOrArrow(name: String): Boolean =
      Picker[A].funcReturnAbilityOrArrow(p, name)

    def funcAcceptAbility(name: String): Boolean =
      Picker[A].funcAcceptAbility(p, name)

    def setImportPaths(importPaths: Map[String, String]): A =
      Picker[A].setImportPaths(p, importPaths)

    def addPart(part: (A, RawPart)): A = Picker[A].addPart(p, part)

    def addParts(parts: List[RawPart]): A =
      parts.foldLeft(p) { case (ctx, part) => ctx.addPart(ctx -> part) }

    def addFreeParts(parts: List[RawPart]): A =
      parts.foldLeft(p) { case (ctx, part) => ctx.addPart(blank -> part) }

    def setModuleName(name: PName): A = Picker[A].setModuleName(p, name)
    def setDeclares(declares: Set[PName]): A = Picker[A].setDeclares(p, declares)
    def setExports(exports: Map[PName, Option[PName]]): A = Picker[A].setExports(p, exports)

    def clearModule: A = Picker[A].clearModule(p)

    def scoped(path: PName): A =
      Picker[A].scoped(p, path)

    def unscoped(path: PName): Option[A] =
      Picker[A].unscoped(p, path)
  }

  private def returnsAbilityOrArrowOrStreamMap(arrowType: ArrowType): Boolean =
    arrowType.codomain.toList.exists {
      case _: AbilityType => true
      case _: ArrowType => true
      case _: StreamMapType => true
      case _ => false
    }

  private def acceptsAbilityOrStreamMap(arrowType: ArrowType): Boolean =
    arrowType.domain.toList.exists {
      case _: AbilityType => true
      case _: StreamMapType => true
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

    override def isAbility(ctx: RawContext, name: String): Boolean =
      ctx.types.get(SName.nameUnsafe(name)).exists(isAbilityType)

    override def funcReturnAbilityOrArrow(ctx: RawContext, name: String): Boolean =
      ctx.funcs
        .get(SName.nameUnsafe(name))
        .map(_.arrow.`type`)
        .exists(returnsAbilityOrArrowOrStreamMap)

    override def funcAcceptAbility(ctx: RawContext, name: String): Boolean =
      ctx.funcs.get(SName.nameUnsafe(name)).map(_.arrow.`type`).exists(acceptsAbilityOrStreamMap)

    override def funcNames(ctx: RawContext): Set[String] =
      ctx.funcs.keySet.map(_.name)

    override def definedAbilityNames(ctx: RawContext): Set[String] =
      ctx.definedAbilities.keySet.map(_.name)

    override def addPart(ctx: RawContext, part: (RawContext, RawPart)): RawContext =
      ctx.copy(parts = ctx.parts :+ part)

    override def moduleName(ctx: RawContext): Option[PName] = ctx.moduleName

    override def declares(ctx: RawContext): Set[PName] = ctx.declares

    override def exports(ctx: RawContext): Map[PName, Option[PName]] = ctx.exports

    override def allNames(ctx: RawContext): Set[String] = ctx.allNames

    // dummy
    override def setImportPaths(ctx: RawContext, importPaths: Map[String, String]): RawContext =
      ctx

    override def setModuleName(ctx: RawContext, name: PName): RawContext =
      RawContext.moduleLens.modify(_.copy(name = name.some))(ctx)

    override def setDeclares(ctx: RawContext, declares: Set[PName]): RawContext =
      RawContext.moduleLens.modify(_.copy(declares = declares))(ctx)

    override def setExports(ctx: RawContext, exports: Map[PName, Option[PName]]): RawContext =
      RawContext.moduleLens.modify(_.copy(exports = exports))(ctx)

    override def clearModule(ctx: RawContext): RawContext =
      RawContext.moduleLens.set(RawContext.Module.blank)(ctx)

    override def scoped(ctx: RawContext, path: PName): RawContext =
      path.parts.toList.foldRight(ctx.clearModule) { case (name, stepCtx) =>
        RawContext.fromAbilities(
          Map(name -> stepCtx)
        )
      }

    override def unscoped(ctx: RawContext, path: PName): Option[RawContext] =
      search(ctx, path).collect { case ctx: RawContext => ctx }

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

        path.fold(inner)(subPath =>
          // TODO:  Should we move parts renaming to `scoped`?
          //        It causes tests to fail for some reason
          inner
            .prependPathToParts(subPath)
            .scoped(subPath)
        )
      }

    override def pickHeader(ctx: RawContext): RawContext =
      RawContext.blank.copy(module = ctx.module) |+|
        ctx.moduleName.map(blank.scoped).orEmpty

    override def pickDeclared(ctx: RawContext): RawContext =
      if (ctx.module.isEmpty) ctx
      else
        ctx.declares.toList
          .flatMap(n => pick(ctx, n, rename = None))
          .foldLeft(pickHeader(ctx))(_ |+| _)

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
  }

  /**
   * This trait is for documentation purposes only.
   * It defines the laws for `Picker` typeclass.
   */
  private trait Laws[A: Picker] {

    private def moduleContainsItself(ctx: A): Boolean =
      ctx.moduleName.isEmpty || ctx.unscoped(ctx.moduleName.get).isDefined

    private def unscopedIsReverseOfScoped(ctx: A, path: PName): Boolean =
      ctx.scoped(path).unscoped(path).contains(ctx)
  }
}
