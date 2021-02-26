package aqua

import aqua.parser.lexer.{Value, VarLambda}
import aqua.parser._
import cats.{Comonad, MonoidK}
import cats.data.NonEmptyList
import cats.syntax.comonad._
import cats.syntax.functor._

// Fully resolved Scope must have no expected abilities (all resolved)
case class Names[F[_]](
  // None means "inherit"
  peerId: Option[F[Value]] = None,
  // Take vars, set vars
  // Data type is not yet known
  importData: Map[String, F[Value]] = Map.empty[String, F[Value]],
  exportData: Map[String, F[String]] = Map.empty[String, F[String]],
  // Abilities can be imported or set
  expectedAbilities: Map[String, F[String]] = Map.empty[String, F[String]],
  resolvedAbilities: Map[String, F[String]] = Map.empty[String, F[String]],
  // We don't know the types yet
  expectArrows: Map[String, F[String]] = Map.empty[String, F[String]],
  // Set when know
  mode: Option[F[Names.CustomMode]] = None
)

object Names {
  sealed trait CustomMode
  case object ParMode extends CustomMode
  case object XorMode extends CustomMode

  private def combineSeq[G[_]: Comonad](a: Names[G], b: Names[G]): Names[G] =
    a.copy(
      exportData = a.exportData ++ b.exportData,
      importData = a.importData ++ b.importData.view.filterKeys(k => !a.exportData.contains(k)).toMap,
      resolvedAbilities = if (b.peerId == a.peerId) a.resolvedAbilities ++ b.resolvedAbilities else a.resolvedAbilities,
      expectedAbilities =
        a.expectedAbilities ++ b.expectedAbilities.view.filterKeys(k => !a.resolvedAbilities.contains(k)).toMap,
      expectArrows = a.expectArrows ++ b.expectArrows
    )

  def funcOps[G[_]: Comonad](ops: NonEmptyList[G[FuncOp[G]]]): Names[G] =
    ops.map(funcOp[G]).foldLeft[Names[G]](Names[G]()) {
      case (acc, n) if n.mode.isEmpty =>
        combineSeq(acc, n)
      case (acc, n) if n.mode.exists(_.extract == ParMode) =>
        acc.copy(
          exportData = acc.exportData ++ n.exportData,
          importData = acc.importData ++ n.importData,
          expectedAbilities = acc.expectedAbilities ++ n.expectedAbilities,
          expectArrows = acc.expectArrows ++ n.expectArrows
        )
      case (acc, n) if n.mode.exists(_.extract == XorMode) =>
        acc.copy(
          importData = acc.importData ++ n.importData,
          expectedAbilities = acc.expectedAbilities ++ n.expectedAbilities,
          expectArrows = acc.expectArrows ++ n.expectArrows
        )
    }

  private def valuesToNames[G[_]: Comonad](args: List[G[Value]]): Map[String, G[Value]] =
    args
      .collect(arg =>
        arg.extract match {
          case VarLambda(name, _) => name -> arg
        }
      )
      .toMap

  def funcOp[G[_]: Comonad](op: G[FuncOp[G]]): Names[G] =
    op.extract match {
      case FuncCall(fname, fargs) =>
        Names[G](
          expectArrows = Map(fname.extract -> fname),
          importData = valuesToNames(fargs)
        )
      case AbilityFuncCall(ab, fc) =>
        val funcNames = funcOp(fc.widen[FuncOp[G]])
        funcNames.copy(expectedAbilities = Map(ab.extract -> ab), expectArrows = Map.empty)
      case Extract(n, fn) =>
        val funcNames = funcOp(fn.widen[FuncOp[G]])
        funcNames.copy(exportData = Map(n.extract -> n))
      case AbilityId(ab, id) =>
        Names[G](resolvedAbilities = Map(ab.extract -> ab), importData = valuesToNames(id :: Nil))
      case On(p, ops) =>
        val ns = funcOps(ops.map(_.widen[FuncOp[G]])).copy(peerId = Some(p))
        p.extract match {
          case VarLambda(name, _) =>
            ns.copy(importData = ns.importData + (name -> p))
          case _ => ns
        }
      case Par(op) =>
        funcOp(op.widen[FuncOp[G]]).copy(mode = Some(op.as(ParMode)))
    }

  def funcHeadNames[G[_]: Comonad](head: FuncHead[G], body: Names[G]): Names[G] =
    head.args.foldLeft(
      body.copy(
        // We clear the mode, as functions are always defined in a sequence
        mode = None,
        // Function may have result type, but it's not names
        exportData = Map.empty,
        // Until we have a notion for exporting abilities, they're cleaned
        resolvedAbilities = Map.empty,
        // Even if peer is defined, it's defined inside
        peerId = None
      )
    ) {
      case (names, (k, (_, ft))) =>
        ft.extract match {
          case _: DataType =>
            names.copy(importData = names.importData - k)
          case _: ArrowType =>
            names.copy(expectArrows = names.expectArrows - k)
        }
    }

  def funcNames[G[_]: Comonad](func: DefFunc[G]): Names[G] =
    funcHeadNames(func.head, funcOps(func.body))

  def blockNames[G[_]: Comonad](block: Block[G]): Names[G] =
    block match {
      case func: DefFunc[G] =>
        funcNames(func)
      case _ =>
        // Until we care about types, there's no imports/exports
        Names[G]()
    }

  def foldVerify[G[_]: Comonad](input: List[Names[G]]): Either[G[String], Names[G]] =
    input.foldLeft[Either[G[String], Names[G]]](Right[G[String], Names[G]](Names[G]())) {
      case (accE, ns) =>
        accE.map(acc => combineSeq[G](acc, ns)).flatMap { acc =>
          val maybeErr: Option[G[String]] = {
            List(
              acc.importData.headOption.map(_._2.as("Unknown variable")),
              acc.expectArrows.headOption.map(_._2.as("Unknown arrow")),
              acc.expectedAbilities.headOption.map(_._2.as("Unresolved ability"))
            ).collectFirst {
              case Some(v) => v
            }
          }

          maybeErr.toLeft(acc)

        }
    }
}
