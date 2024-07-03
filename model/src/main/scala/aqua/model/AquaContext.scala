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

package aqua.model

import aqua.helpers.data.{PName, SName}
import aqua.raw.arrow.FuncRaw
import aqua.raw.ops.CallArrowRawTag
import aqua.raw.value.CallArrowRaw
import aqua.raw.value.ValueRaw
import aqua.raw.{ConstantRaw, RawContext, RawPart, ServiceRaw, TypeRaw}
import aqua.types.{AbilityType, StructType, Type}

import cats.Monoid
import cats.data.{Chain, NonEmptyMap, State}
import cats.kernel.Semigroup
import cats.syntax.applicative.*
import cats.syntax.bifunctor.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.monoid.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import scala.collection.immutable.SortedMap
import scribe.Logging

case class AquaContext(
  module: Option[String],
  funcs: Map[String, FuncArrow],
  types: Map[String, Type],
  values: Map[String, ValueModel],
  abilities: Map[SName, AquaContext],
  // TODO: merge this with abilities, when have ability resolution variance
  services: Map[String, ServiceModel]
) {

  // TODO: it's a duplicate
  private def all[T](
    what: AquaContext => Map[String, T]
  ): Map[String, T] = {
    def helper(ctx: AquaContext): Map[PName, T] =
      what(ctx).map { case (name, v) => PName.simpleUnsafe(name) -> v } ++
        ctx.abilities.flatMap { case (aname, ab) =>
          helper(ab).map { case (name, v) => name.prefixed(aname) -> v }
        }

    helper(this).map { case (name, v) => name.value -> v }
  }

  lazy val allServices: Map[String, ServiceModel] =
    all(_.services)

  lazy val allValues: Map[String, ValueModel] =
    all(_.values) ++
      /**
       * Add values from services that have default ID
       * So that they will be available in functions.
       */
      allServices.flatMap { case (srvName, srv) =>
        srv.defaultId.toList.flatMap(_ =>
          srv.`type`.arrows.map { case (arrowName, arrowType) =>
            val fullName = AbilityType.fullName(srvName, arrowName)

            fullName -> VarModel(
              fullName,
              arrowType
            )
          }.updated(
            srvName,
            VarModel(
              srvName,
              srv.`type`
            )
          )
        )
      }

  lazy val allFuncs: Map[String, FuncArrow] =
    all(_.funcs) ++
      /**
       * Add functions from services that have default ID
       * So that they will be available in functions.
       */
      allServices.flatMap { case (srvName, srv) =>
        srv.defaultId.toList.flatMap(id =>
          srv.`type`.arrows.map { case (arrowName, arrowType) =>
            val fullName = AbilityType.fullName(srvName, arrowName)

            fullName -> FuncArrow.fromServiceMethod(
              fullName,
              srvName,
              arrowName,
              arrowType,
              id
            )
          }
        )
      }

  private def pickOne[N, T](
    name: N,
    newName: N,
    ctx: Map[N, T],
    add: (AquaContext, Map[N, T]) => AquaContext
  ): AquaContext = ctx
    .get(name)
    .fold(AquaContext.blank)(t =>
      add(
        AquaContext.blank,
        Map(newName -> t)
      )
    )

  def pick(name: PName, rename: SName): AquaContext =
    name.uncons match {
      case (name, Some(subpath)) => abilities.get(name).map(_.pick(subpath, rename)).orEmpty
      case (name, None) =>
        pickOne(name.name, rename.name, funcs, (ctx, el) => ctx.copy(funcs = el)) |+|
          pickOne(name.name, rename.name, types, (ctx, el) => ctx.copy(types = el)) |+|
          pickOne(name.name, rename.name, values, (ctx, el) => ctx.copy(values = el)) |+|
          pickOne(name, rename, abilities, (ctx, el) => ctx.copy(abilities = el)) |+|
          pickOne(name.name, rename.name, services, (ctx, el) => ctx.copy(services = el))

    }

  def withModule(newModule: Option[String]): AquaContext =
    copy(module = newModule)

  def withAbilities(newAbilities: Map[SName, AquaContext]): AquaContext =
    copy(abilities = newAbilities)

  def withServices(newServices: Map[String, ServiceModel]): AquaContext =
    copy(services = newServices)

  def withValues(newValues: Map[String, ValueModel]): AquaContext =
    copy(values = newValues)

  def withFuncs(newFuncs: Map[String, FuncArrow]): AquaContext =
    copy(funcs = newFuncs)

  def withTypes(newTypes: Map[String, Type]): AquaContext =
    copy(types = newTypes)

  override def toString(): String =
    s"AquaContext(" +
      s"module=$module, " +
      s"funcs=${funcs.keys}, " +
      s"types=${types.keys}, " +
      s"values=${values.keys}, " +
      s"abilities=${abilities.keys}, " +
      s"services=${services.keys})"
}

object AquaContext extends Logging {

  case class Cache private (
    private val data: Map[Cache.RefKey[RawContext], AquaContext]
  ) {
    lazy val size: Long = data.size

    private def get(ctx: RawContext): Option[AquaContext] =
      data.get(Cache.RefKey(ctx))

    private def updated(ctx: RawContext, aCtx: AquaContext): Cache =
      copy(data = data.updated(Cache.RefKey(ctx), aCtx))
  }

  type Cached[A] = State[Cache, A]

  object Cache {

    val empty: Cache = Cache(Map.empty)

    def get(ctx: RawContext): Cached[Option[AquaContext]] =
      State.inspect(_.get(ctx))

    def updated(ctx: RawContext, aCtx: AquaContext): Cached[Unit] =
      State.modify(_.updated(ctx, aCtx))

    private class RefKey[T <: AnyRef](val ref: T) extends AnyVal {

      override def equals(other: Any): Boolean = other match {
        case that: RefKey[_] => that.ref eq ref
      }

      override def hashCode(): Int = System.identityHashCode(ref)
    }
  }

  val blank: AquaContext =
    AquaContext(None, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

  given Monoid[AquaContext] with {

    val empty: AquaContext =
      blank

    def combine(x: AquaContext, y: AquaContext): AquaContext =
      AquaContext(
        x.module orElse y.module,
        x.funcs ++ y.funcs,
        x.types ++ y.types,
        x.values ++ y.values,
        x.abilities ++ y.abilities,
        x.services ++ y.services
      )
  }

  // Convert RawContext into AquaContext, with exports handled
  def exportsFromRaw(raw: RawContext): Cached[AquaContext] = for {
    ctx <- fromRawContext(raw)
    handled = raw.exports.toList
      .foldMap(ctx.pick.tupled)
      .withModule(ctx.module)
  } yield handled

  // Convert RawContext into AquaContext, with no exports handled
  private def fromRawContext(raw: RawContext): Cached[AquaContext] =
    Cache
      .get(raw)
      .flatMap {
        case Some(aCtx) => aCtx.pure
        case None =>
          for {
            init <- raw.abilities.toList.traverse { case (sname, ab) =>
              fromRawContext(ab).map(sname -> _)
            }.map(abs => blank.withAbilities(abs.toMap))
            parts <- raw.parts.foldMapM(handlePart.tupled)
          } yield init |+| parts
      }
      .flatTap(aCtx => Cache.updated(raw, aCtx))

  private def handlePart(raw: RawContext, part: RawPart): Cached[AquaContext] =
    part match {
      case c: ConstantRaw =>
        // Just saving a constant
        // Actually this should have no effect, as constants are resolved by semantics
        fromRawContext(raw).map(pctx =>
          blank.withValues(
            if (c.allowOverrides && pctx.values.contains(c.name)) Map.empty
            else Map(c.name -> ValueModel.fromRaw(c.value).resolveWith(pctx.allValues))
          )
        )

      case func: FuncRaw =>
        fromRawContext(raw).map(pctx =>
          blank.withFuncs(
            Map(
              func.name -> FuncArrow.fromRaw(
                raw = func,
                arrows = pctx.allFuncs,
                constants = pctx.allValues,
                topology = None
              )
            )
          )
        )

      case t: TypeRaw =>
        // Just remember the type (why? it's can't be exported, so seems useless)
        blank.withTypes(Map(t.name -> t.`type`)).pure

      case m: ServiceRaw =>
        // To add a service, we need to resolve its ID, if any
        fromRawContext(raw).map { pctx =>
          val id = m.defaultId
            .map(ValueModel.fromRaw)
            .map(_.resolveWith(pctx.allValues))
          val srv = ServiceModel(m.name, m.`type`, id)

          blank
            .withAbilities(
              m.defaultId
                .map(id => Map(SName.nameUnsafe(m.name) -> fromService(m, id)))
                .orEmpty
            )
            .withServices(Map(m.name -> srv))
        }

      case _ => blank.pure
    }

  private def fromService(sm: ServiceRaw, serviceId: ValueRaw): AquaContext =
    blank
      .withModule(Some(sm.name))
      .withFuncs(sm.`type`.arrows.map { case (fnName, arrowType) =>
        fnName -> FuncArrow.fromServiceMethod(
          fnName,
          sm.name,
          fnName,
          arrowType,
          serviceId
        )
      })
}
