package aqua.raw

import aqua.helpers.data.PName
import aqua.helpers.data.SName
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
  module: Option[PName] = None,
  declares: Set[PName] = Set.empty,
  exports: Map[PName, Option[PName]] = Map.empty,
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

  // override def toString: String =
  //   s"""|module: ${module.map(_.value).getOrElse("unnamed")}
  //       |declares: ${declares.map(_.value).mkString(", ")}
  //       |exports: ${exports.map { case (name, rename) =>
  //     rename.fold(name.value)(name.value + " as " + _.value)
  //   }.mkString(", ")}
  //       |parts: ${parts.map { case (_, part) => part.name }.toList.mkString(", ")}
  //       |abilities: ${abilities.keys.map(_.name).mkString(", ")}""".stripMargin

  override def toString(): String = debug

  def debug: String = {
    val abs = abilities.map { case (name, ab) =>
      s"${name.name}: (${ab.debug})"
    }

    s"""|module: ${module.map(_.value).getOrElse("unnamed")}
        |declares: ${declares.map(_.value).mkString(", ")}
        |parts: ${parts.map { case (_, part) => part.name }.toList.mkString(", ")}
        |abilities: ${abs.mkString}
    """.stripMargin
  }
}

object RawContext {
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
        x.module orElse y.module,
        x.declares ++ y.declares,
        x.exports ++ y.exports,
        x.parts ++ y.parts,
        // This combines abilities (which are RawContexts too) recursively
        x.abilities.alignCombine(y.abilities)
      )
  }
}
