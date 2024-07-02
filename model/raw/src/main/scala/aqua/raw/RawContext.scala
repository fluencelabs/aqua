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

package aqua.raw

import aqua.errors.Errors.internalError
import aqua.helpers.data.{PName, SName}
import aqua.raw.arrow.FuncRaw
import aqua.raw.value.ValueRaw
import aqua.types.{AbilityType, StructType, Type}

import cats.Eval
import cats.Monoid
import cats.Semigroup
import cats.data.Chain
import cats.data.NonEmptyMap
import cats.syntax.align.*
import cats.syntax.monoid.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import monocle.Lens
import monocle.macros.GenLens
import scala.collection.immutable.SortedMap

/**
 * RawContext is essentially a model of the source code – the first one we get to from the AST.
 *
 * @param module
 * Information about module this context represents
 * @param parts
 * Declarations, functions, etc of the file
 * @param abilities
 * Abilities (e.g. used contexts) available in the scope
 */
case class RawContext(
  module: RawContext.Module = RawContext.Module.blank,
  parts: RawContext.Parts = Chain.empty,
  abilities: Map[SName, RawContext] = Map.empty
) {

  def isEmpty: Boolean = this == RawContext.blank

  def nonEmpty: Boolean = !isEmpty

  private def collectPartsMap[T](f: PartialFunction[RawPart, T]): Map[SName, T] =
    parts.collect {
      case (_, p) if f.isDefinedAt(p) =>
        SName.nameUnsafe(p.name) -> f(p)
    }.toList.toMap

  private def all[T](what: RawContext => Map[SName, T]): Map[PName, T] =
    what(this).map { case (p, t) => p.toPName -> t } ++
      abilities.flatMap { case (name, ab) =>
        ab.all(what).map { case (p, t) => p.prefixed(name) -> t }
      }

  lazy val allAbilities: Map[PName, RawContext] =
    all(_.abilities)

  lazy val services: Map[SName, ServiceRaw] =
    collectPartsMap { case srv: ServiceRaw =>
      srv
    }

  lazy val allServices: Map[PName, ServiceRaw] =
    all(_.services)

  lazy val types: Map[SName, Type] =
    collectPartsMap {
      case t: TypeRaw => t.`type`
      case s: ServiceRaw => s.`type`
    }

  lazy val allTypes: Map[PName, Type] =
    all(_.types)

  lazy val funcs: Map[SName, FuncRaw] =
    collectPartsMap { case f: FuncRaw =>
      f
    }

  lazy val allFuncs: Map[PName, FuncRaw] =
    all(_.funcs)

  lazy val values: Map[SName, ValueRaw] =
    collectPartsMap { case c: ConstantRaw =>
      c.value
    }

  lazy val allValues: Map[PName, ValueRaw] =
    all(_.values)

  lazy val definedAbilities: Map[SName, AbilityType] =
    collectPartsMap { case TypeRaw(_, at: AbilityType) =>
      at
    }

  lazy val allDefinedAbilities: Map[PName, AbilityType] =
    all(_.definedAbilities)

  lazy val allNames: Set[String] =
    // TODO: How about names in abilities?
    parts.map { case (_, p) => p.name }.toList.toSet

  lazy val moduleName: Option[PName] = module.name

  lazy val declares: Set[PName] = module.declares

  lazy val exports: Map[PName, Option[PName]] = module.exports

  def prependPathToParts(path: PName): RawContext =
    RawContext.partsLens.modify(
      _.map { case (partContext, part) =>
        (partContext, part.addAbilityName(path.value))
      }
    )(this)

  override def toString: String = {
    val exportsStr = exports.map { case (name, rename) =>
      rename.fold(name.value)(name.value + " as " + _.value)
    }.mkString(", ")
    val partsStr = parts.map { case (_, part) => part.name }.toList.mkString(", ")
    val abilitiesStr = abilities.keys.map(_.name).mkString(", ")

    s"""|$module
        |declares: ${declares.map(_.value).mkString(", ")}
        |exports: $exportsStr
        |parts: $partsStr
        |abilities: $abilitiesStr""".stripMargin
  }
}

object RawContext {

  final case class Module(
    name: Option[PName] = None,
    declares: Set[PName] = Set.empty,
    exports: Map[PName, Option[PName]] = Map.empty
  ) {

    // Module is considered empty if it has no name
    def isEmpty: Boolean = name.isEmpty

    override def toString: String = {
      val nameStr = name.fold("<unnamed>")(_.value)
      val declaresStr = declares.map(_.value).mkString(", ")
      val exportsStr = exports.map { case (name, rename) =>
        rename.fold(name.value)(name.value + " as " + _.value)
      }.mkString(", ")

      s"""|module: $nameStr
          |declares: $declaresStr
          |exports: $exportsStr""".stripMargin
    }
  }

  object Module {
    lazy val blank: Module = Module()

    given Monoid[Module] with {

      override def empty: Module = blank

      override def combine(x: Module, y: Module): Module =
        Module(
          name = x.name.orElse(y.name),
          declares = x.declares ++ y.declares,
          exports = x.exports ++ y.exports
        )
    }
  }

  lazy val moduleLens: Lens[RawContext, RawContext.Module] = GenLens[RawContext](_.module)

  type Parts = Chain[(RawContext, RawPart)];

  val blank: RawContext = RawContext()

  val partsLens: Lens[RawContext, Chain[(RawContext, RawPart)]] =
    GenLens[RawContext](_.parts)

  val abilitiesLens: Lens[RawContext, Map[SName, RawContext]] =
    GenLens[RawContext](_.abilities)

  def fromParts(parts: Chain[(RawContext, RawPart)]): RawContext =
    partsLens.set(parts)(blank)

  def fromAbilities(abilities: Map[SName, RawContext]): RawContext =
    abilitiesLens.set(abilities)(blank)

  given Monoid[RawContext] with {

    override def empty: RawContext = blank

    override def combine(x: RawContext, y: RawContext): RawContext =
      RawContext(
        x.module |+| y.module,
        x.parts |+| y.parts,
        // This combines abilities (which are RawContexts too) recursively
        x.abilities.alignCombine(y.abilities)
      )
  }
}
