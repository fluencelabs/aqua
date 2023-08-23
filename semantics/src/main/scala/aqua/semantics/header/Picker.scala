package aqua.semantics.header

import aqua.raw.{RawContext, RawPart}
import aqua.types.{AbilityType, ArrowType, Type}
import cats.Semigroup
import cats.syntax.semigroup.*

// Able to pick info from different contexts
trait Picker[A] {

  def all(ctx: A): Set[String]
  def funcNames(ctx: A): List[String]
  def blank: A
  def pick(ctx: A, name: String, rename: Option[String], declared: Boolean): Option[A]
  def pickDeclared(ctx: A)(implicit semi: Semigroup[A]): A
  def pickHeader(ctx: A): A
  def module(ctx: A): Option[String]
  def exports(ctx: A): Option[Map[String, Option[String]]]
  def isAbility(ctx: A, name: String): Boolean
  def funcReturnAbilityOrArrow(ctx: A, name: String): Boolean
  def funcAcceptAbility(ctx: A, name: String): Boolean
  def declares(ctx: A): Set[String]
  def setAbility(ctx: A, name: String, ctxAb: A): A
  def setModule(ctx: A, name: Option[String], declares: Set[String]): A
  def setExports(ctx: A, exports: Map[String, Option[String]]): A
  def setInit(ctx: A, ctxInit: Option[A]): A
  def addPart(ctx: A, part: (A, RawPart)): A
}

object Picker {

  extension [A: Picker](p: A) {

    def blank: A = Picker[A].blank
    def all: Set[String] = Picker[A].all(p)
    def funcNames: List[String] = Picker[A].funcNames(p)

    def pick(name: String, rename: Option[String], declared: Boolean): Option[A] =
      Picker[A].pick(p, name, rename, declared)
    def pickDeclared(implicit semi: Semigroup[A]): A = Picker[A].pickDeclared(p)
    def pickHeader: A = Picker[A].pickHeader(p)
    def module: Option[String] = Picker[A].module(p)
    def exports: Option[Map[String, Option[String]]] = Picker[A].exports(p)

    def isAbility(name: String): Boolean = Picker[A].isAbility(p, name)

    def funcReturnAbilityOrArrow(name: String): Boolean =
      Picker[A].funcReturnAbilityOrArrow(p, name)
    def funcAcceptAbility(name: String): Boolean = Picker[A].funcAcceptAbility(p, name)
    def declares: Set[String] = Picker[A].declares(p)
    def setAbility(name: String, ctx: A): A = Picker[A].setAbility(p, name, ctx)
    def setInit(ctx: Option[A]): A = Picker[A].setInit(p, ctx)
    def addPart(part: (A, RawPart)): A = Picker[A].addPart(p, part)

    def setModule(name: String, declares: Set[String]): A =
      Picker[A].setModule(p, Some(name), declares)

    def setOptModule(name: Option[String], declares: Set[String]): A =
      Picker[A].setModule(p, name, declares)

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
    override def exports(ctx: RawContext): Option[Map[String, Option[String]]] = ctx.exports

    override def isAbility(ctx: RawContext, name: String): Boolean =
      ctx.types.get(name).exists(isAbilityType)

    override def funcReturnAbilityOrArrow(ctx: RawContext, name: String): Boolean =
      ctx.funcs.get(name).map(_.arrow.`type`).exists(returnsAbilityOrArrow)

    override def funcAcceptAbility(ctx: RawContext, name: String): Boolean =
      ctx.funcs.get(name).map(_.arrow.`type`).exists(acceptsAbility)

    override def funcNames(ctx: RawContext): List[String] = ctx.funcs.keys.toList

    override def addPart(ctx: RawContext, part: (RawContext, RawPart)): RawContext =
      ctx.copy(parts = ctx.parts :+ part)

    override def setInit(ctx: RawContext, ctxInit: Option[RawContext]): RawContext =
      ctx.copy(init = ctxInit)

    override def all(ctx: RawContext): Set[String] =
      ctx.`type`("").map(_.fields.toNel.map(_._1).toList.toSet).getOrElse(Set.empty)
    override def module(ctx: RawContext): Option[String] = ctx.module
    override def declares(ctx: RawContext): Set[String] = ctx.declares

    override def setAbility(ctx: RawContext, name: String, ctxAb: RawContext): RawContext =
      ctx.copy(abilities = Map(name -> ctxAb))

    override def setModule(
      ctx: RawContext,
      name: Option[String],
      declares: Set[String]
    ): RawContext =
      ctx.copy(module = name, declares = declares)

    override def setExports(ctx: RawContext, exports: Map[String, Option[String]]): RawContext =
      ctx.copy(exports = Some(exports))

    override def pick(
      ctx: RawContext,
      name: String,
      rename: Option[String],
      declared: Boolean
    ): Option[RawContext] =
      Option
        .when(!declared || ctx.declares(name)) {
          RawContext.blank
            .copy(parts = ctx.parts.filter(_._2.name == name).map { case (partContext, part) =>
              (partContext, rename.fold(part)(part.rename))
            })
        }
        .filter(_.nonEmpty)

    override def pickHeader(ctx: RawContext): RawContext =
      RawContext.blank.copy(module = ctx.module, declares = ctx.declares, exports = ctx.exports)

    override def pickDeclared(ctx: RawContext)(implicit semi: Semigroup[RawContext]): RawContext =
      if (ctx.module.isEmpty) ctx
      else
        ctx.declares.toList
          .flatMap(n => pick(ctx, n, None, ctx.module.nonEmpty))
          .foldLeft(pickHeader(ctx))(
            _ |+| _
          )
  }

}
