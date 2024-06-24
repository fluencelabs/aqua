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

import aqua.helpers.data.PName
import aqua.raw.{RawContext, RawPart}
import aqua.types.{AbilityType, ArrowType, StreamMapType, Type}
import cats.Semigroup
import cats.syntax.foldable.*
import cats.syntax.semigroup.*

// Able to pick info from different contexts
trait Picker[A] {
  def funcNames(ctx: A): Set[String]
  def definedAbilityNames(ctx: A): Set[String]
  def blank: A
  def pick(ctx: A, name: String, rename: Option[String], declared: Boolean): Option[A]
  def pick(ctx: A, name: PName, declared: Boolean): Option[A]
  def pickDeclared(ctx: A): A
  def pickHeader(ctx: A): A
  def module(ctx: A): Option[String]
  def allNames(ctx: A): Set[String]
  def declaredNames(ctx: A): Set[String]
  def exports(ctx: A): Map[String, Option[String]]
  def isAbility(ctx: A, name: String): Boolean
  def funcReturnAbilityOrArrow(ctx: A, name: String): Boolean
  def funcAcceptAbility(ctx: A, name: String): Boolean
  def setAbility(ctx: A, name: String, ctxAb: A): A
  def setImportPaths(ctx: A, importPaths: Map[String, String]): A
  def setModule(ctx: A, name: Option[String]): A
  def setDeclares(ctx: A, declares: Set[PName]): A
  def setExports(ctx: A, exports: Map[String, Option[String]]): A
  def addPart(ctx: A, part: (A, RawPart)): A
}

object Picker {

  extension [A: Picker](p: A) {

    def blank: A = Picker[A].blank

    def funcNames: Set[String] = Picker[A].funcNames(p)
    def definedAbilityNames: Set[String] = Picker[A].definedAbilityNames(p)

    def pick(name: String, rename: Option[String], declared: Boolean): Option[A] =
      Picker[A].pick(p, name, rename, declared)

    def pick(name: PName, declared: Boolean): Option[A] =
      Picker[A].pick(p, name, declared)

    def pickDeclared: A = Picker[A].pickDeclared(p)
    def pickHeader: A = Picker[A].pickHeader(p)
    def module: Option[String] = Picker[A].module(p)
    def declaredNames: Set[String] = Picker[A].declaredNames(p)
    def allNames: Set[String] = Picker[A].allNames(p)
    def exports: Map[String, Option[String]] = Picker[A].exports(p)

    def isAbility(name: String): Boolean = Picker[A].isAbility(p, name)

    def funcReturnAbilityOrArrow(name: String): Boolean =
      Picker[A].funcReturnAbilityOrArrow(p, name)
    def funcAcceptAbility(name: String): Boolean = Picker[A].funcAcceptAbility(p, name)
    def setAbility(name: String, ctx: A): A = Picker[A].setAbility(p, name, ctx)

    def setImportPaths(importPaths: Map[String, String]): A =
      Picker[A].setImportPaths(p, importPaths)
    def addPart(part: (A, RawPart)): A = Picker[A].addPart(p, part)

    def addParts(parts: List[RawPart]): A =
      parts.foldLeft(p) { case (ctx, part) => ctx.addPart(ctx -> part) }

    def addFreeParts(parts: List[RawPart]): A =
      parts.foldLeft(p) { case (ctx, part) => ctx.addPart(blank -> part) }

    def setModule(name: Option[String]): A =
      Picker[A].setModule(p, name)

    def setDeclares(declares: Set[PName]): A =
      Picker[A].setDeclares(p, declares)

    def setExports(exports: Map[String, Option[String]]): A =
      Picker[A].setExports(p, exports)
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
    override def exports(ctx: RawContext): Map[String, Option[String]] = ctx.exports

    override def isAbility(ctx: RawContext, name: String): Boolean =
      ctx.types.get(name).exists(isAbilityType)

    override def funcReturnAbilityOrArrow(ctx: RawContext, name: String): Boolean =
      ctx.funcs.get(name).map(_.arrow.`type`).exists(returnsAbilityOrArrowOrStreamMap)

    override def funcAcceptAbility(ctx: RawContext, name: String): Boolean =
      ctx.funcs.get(name).map(_.arrow.`type`).exists(acceptsAbilityOrStreamMap)

    override def funcNames(ctx: RawContext): Set[String] = ctx.funcs.keySet

    override def definedAbilityNames(ctx: RawContext): Set[String] = ctx.definedAbilities.keySet

    override def addPart(ctx: RawContext, part: (RawContext, RawPart)): RawContext =
      ctx.copy(parts = ctx.parts :+ part)

    override def module(ctx: RawContext): Option[String] = ctx.module

    override def declaredNames(ctx: RawContext): Set[String] = ctx.declaredNames

    override def allNames(ctx: RawContext): Set[String] = ctx.allNames

    override def setAbility(ctx: RawContext, name: String, ctxAb: RawContext): RawContext =
      ctx.copy(abilities = Map(name -> ctxAb.copy(parts = ctxAb.parts.map {
        case (partContext, part) =>
          (partContext, part.addAbilityName(name))
      })))

    // dummy
    override def setImportPaths(ctx: RawContext, importPaths: Map[String, String]): RawContext =
      ctx

    override def setModule(ctx: RawContext, name: Option[String]): RawContext =
      ctx.copy(module = name)

    override def setDeclares(ctx: RawContext, declares: Set[PName]): RawContext =
      ctx.copy(declares = declares)

    override def setExports(ctx: RawContext, exports: Map[String, Option[String]]): RawContext =
      ctx.copy(exports = exports)

    override def pick(
      ctx: RawContext,
      name: String,
      rename: Option[String],
      declared: Boolean
    ): Option[RawContext] =
      Option
        .when(!declared || ctx.declaredNames(name)) {
          RawContext.fromParts(
            ctx.parts.collect {
              case (partContext, part) if part.name == name =>
                (partContext, rename.fold(part)(part.rename))
            }
          )
        }
        .filter(_.nonEmpty)
        .map(
          // Module and declares should not be lost when picking
          // Because it affects later logic
          _.setModule(ctx.module).setDeclares(Set(PName.simpleUnsafe(name)))
        )

    override def pick(
      ctx: RawContext,
      name: PName,
      declared: Boolean
    ): Option[RawContext] =
      name.simple.fold(
        name.splits.collectFirstSome { case (ab, field) =>
          for {
            ability <- ctx.abilities.get(ab.value)
            inner <- pick(ability, field, declared)
          } yield RawContext
            .fromAbilities(Map(ab.value -> inner))
            // Module and declares should not be lost when picking
            // Because it affects later logic
            .setModule(ctx.module)
            .setDeclares(Set(name))
        }
      )(pick(ctx, _, None, declared))

    override def pickHeader(ctx: RawContext): RawContext =
      RawContext.blank.copy(module = ctx.module, declares = ctx.declares, exports = ctx.exports)

    override def pickDeclared(ctx: RawContext): RawContext =
      if (ctx.module.isEmpty) ctx
      else
        ctx.declares.toList
          .flatMap(n => pick(ctx, n, ctx.module.nonEmpty))
          .foldLeft(pickHeader(ctx))(_ |+| _)
  }

}
