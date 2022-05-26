package aqua.semantics.header

import aqua.raw.{RawContext, RawPart}
import cats.Semigroup
import cats.syntax.semigroup.*

trait Picker[A] {

  def pick(name: String, rename: Option[String], declared: Boolean): Option[A]
  def pickDeclared(implicit semi: Semigroup[A]): A
  def pickHeader: A
  def module: Option[String]
  def declares: Set[String]
}

final class PickerOps[A: Picker](p: A) {

  def pick(name: String, rename: Option[String], declared: Boolean): Option[A] =
    Picker[A].pick(name, rename, declared)
  def pickDeclared(implicit semi: Semigroup[A]): A = Picker[A].pickDeclared
  def pickHeader: A = Picker[A].pickHeader
  def module: Option[String] = Picker[A].module
  def declares: Set[String] = Picker[A].declares
}

object Picker {

  implicit final def apply[A](implicit ev: Picker[A]): Picker[A] = ev

  implicit final def syntaxPicker[A: Picker](a: A): PickerOps[A] =
    new PickerOps[A](a)

  implicit final def rawContextPicker(ctx: RawContext): Picker[RawContext] =
    new PickerRawContext(ctx)

}

class PickerRawContext(ctx: RawContext) extends Picker[RawContext] {

  override def module: Option[String] = ctx.module
  override def declares: Set[String] = ctx.declares

  override def pick(
    name: String,
    rename: Option[String],
    declared: Boolean = ctx.module.nonEmpty
  ): Option[RawContext] =
    Option
      .when(!declared || ctx.declares(name)) {
        RawContext.blank
          .copy(parts = ctx.parts.filter(_._2.name == name).map { case (partContext, part) =>
            (partContext, rename.fold(part)(part.rename))
          })
      }
      .filter(_.nonEmpty)

  override def pickHeader: RawContext =
    RawContext.blank.copy(module = ctx.module, declares = ctx.declares, exports = ctx.exports)

  override def pickDeclared(implicit semi: Semigroup[RawContext]): RawContext =
    if (ctx.module.isEmpty) ctx
    else
      ctx.declares.toList
        .flatMap(pick(_, None))
        .foldLeft(pickHeader)(
          _ |+| _
        )
}
