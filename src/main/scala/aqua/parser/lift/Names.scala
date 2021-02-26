package aqua.parser.lift

import aqua.parser.{AbilityFuncCall, AbilityId, DataType, ExecOp, Extract, FuncCall, FuncOp, On, Par}
import aqua.parser.lexer.{Value, VarLambda}
import cats.Comonad
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

  def funcOps[G[_]: Comonad](ops: NonEmptyList[G[FuncOp[G]]]): Names[G] =
    ops.map(funcOp[G]).foldLeft[Names[G]](Names[G]()) {
      case (acc, n) if n.mode.isEmpty =>
        acc.copy(
          exportData = acc.exportData ++ n.exportData,
          importData = acc.importData ++ n.importData.view.filterKeys(k => !acc.exportData.contains(k)).toMap,
          resolvedAbilities =
            if (n.peerId == acc.peerId) acc.resolvedAbilities ++ n.resolvedAbilities else acc.resolvedAbilities,
          expectedAbilities =
            acc.expectedAbilities ++ n.expectedAbilities.view.filterKeys(k => !acc.resolvedAbilities.contains(k)).toMap,
          expectArrows = acc.expectArrows ++ n.expectArrows
        )
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

  def funcOp[G[_]: Comonad](op: G[FuncOp[G]]): Names[G] =
    op.extract match {
      case FuncCall(fname, fargs) =>
        Names[G](
          expectArrows = Map(fname.extract -> fname),
          importData = fargs
            .collect(arg =>
              arg.extract match {
                case VarLambda(name, _) => name -> arg
              }
            )
            .toMap
        )
      case AbilityFuncCall(ab, fc) =>
        val funcNames = funcOp(fc.widen[FuncOp[G]])
        funcNames.copy(expectedAbilities = Map(ab.extract -> ab))
      case Extract(n, fn) =>
        val funcNames = funcOp(fn.widen[FuncOp[G]])
        funcNames.copy(exportData = Map(n.extract -> n))
      case AbilityId(ab, _) =>
        Names[G](resolvedAbilities = Map(ab.extract -> ab))
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
}
