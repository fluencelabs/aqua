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

import aqua.helpers.data.PName
import aqua.raw.arrow.FuncRaw
import aqua.raw.value.ValueRaw
import aqua.types.{AbilityType, StructType, Type}

import cats.Monoid
import cats.Semigroup
import cats.data.Chain
import cats.data.NonEmptyMap
import cats.syntax.align.*
import cats.syntax.monoid.*
import cats.syntax.option.*
import monocle.Lens
import monocle.macros.GenLens
import scala.collection.immutable.SortedMap

/**
 * RawContext is essentially a model of the source code – the first one we get to from the AST.
 *
 * @param module
 * This file's module name
 * @param declares
 * What this file declares
 * @param exports
 * Defined exports: what to export, and optionally how to rename it
 * @param parts
 * Declarations, functions, etc of the file
 * @param abilities
 * Abilities (e.g. used contexts) available in the scope
 */
case class RawContext(
  module: Option[String] = None,
  declares: Set[PName] = Set.empty,
  exports: Map[String, Option[String]] = Map.empty,
  parts: Chain[(RawContext, RawPart)] = Chain.empty,
  abilities: Map[String, RawContext] = Map.empty
) {

  def isEmpty: Boolean = this == RawContext.blank

  def nonEmpty: Boolean = !isEmpty

  private def prefixFirst[T](prefix: String, pair: (String, T)): (String, T) =
    (prefix + pair._1, pair._2)

  private def collectPartsMap[T](f: PartialFunction[RawPart, T]): Map[String, T] =
    parts.collect { case (_, p) if f.isDefinedAt(p) => p.name -> f(p) }.toList.toMap

  private def all[T](what: RawContext => Map[String, T], prefix: String = ""): Map[String, T] =
    abilities
      .foldLeft(what(this)) { case (ts, (k, v)) =>
        ts ++ v.all(what, k + ".")
      }
      .map(prefixFirst(prefix, _))

  lazy val allAbilities: Map[String, RawContext] =
    all(_.abilities)

  lazy val services: Map[String, ServiceRaw] = collectPartsMap { case srv: ServiceRaw => srv }

  lazy val allServices: Map[String, ServiceRaw] =
    all(_.services)

  lazy val types: Map[String, Type] =
    collectPartsMap {
      case t: TypeRaw => t.`type`
      case s: ServiceRaw => s.`type`
    }

  lazy val allTypes: Map[String, Type] =
    all(_.types)

  lazy val funcs: Map[String, FuncRaw] =
    collectPartsMap { case f: FuncRaw =>
      f
    }

  lazy val allFuncs: Map[String, FuncRaw] =
    all(_.funcs)

  lazy val values: Map[String, ValueRaw] =
    collectPartsMap { case c: ConstantRaw =>
      c.value
    }

  lazy val allValues: Map[String, ValueRaw] = all(_.values)

  lazy val definedAbilities: Map[String, AbilityType] =
    collectPartsMap { case TypeRaw(_, at: AbilityType) => at }

  lazy val allDefinedAbilities: Map[String, AbilityType] =
    all(_.definedAbilities)

  lazy val allNames: Set[String] =
    // TODO: How about names in abilities?
    parts.map { case (_, p) => p.name }.toList.toSet

  lazy val declaredNames: Set[String] =
    declares.map(_.value).toSet

  override def toString: String =
    s"""|module: ${module.getOrElse("unnamed")}
        |declares: ${declares.mkString(", ")}
        |exports: ${exports.map { case (name, rename) =>
      rename.fold(name)(name + " as " + _)
    }.mkString(", ")}
        |parts: ${parts.map { case (_, part) => part.name }.toList.mkString(", ")}
        |abilities: ${abilities.keys.mkString(", ")}""".stripMargin
}

object RawContext {
  val blank: RawContext = RawContext()

  val partsLens: Lens[RawContext, Chain[(RawContext, RawPart)]] =
    GenLens[RawContext](_.parts)

  val abilitiesLens: Lens[RawContext, Map[String, RawContext]] =
    GenLens[RawContext](_.abilities)

  def fromParts(parts: Chain[(RawContext, RawPart)]): RawContext =
    partsLens.set(parts)(blank)

  def fromAbilities(abilities: Map[String, RawContext]): RawContext =
    abilitiesLens.set(abilities)(blank)

  given Monoid[RawContext] with {

    override def empty: RawContext = blank

    override def combine(x: RawContext, y: RawContext): RawContext =
      RawContext(
        x.module orElse y.module,
        x.declares ++ y.declares,
        x.exports ++ y.exports,
        x.parts ++ y.parts,
        // This combines abilities (which are RawContexts too) recursively
        x.abilities.alignCombine(y.abilities)
      )
  }
}
