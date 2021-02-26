package aqua.parser.lift

import aqua.parser.{AbilityFuncCall, AbilityId, DataType, ExecOp, Extract, FuncCall, FuncOp, On, Par}
import aqua.parser.lexer.{Value, VarLambda}
import cats.Comonad
import cats.data.NonEmptyList
import cats.syntax.comonad._
import cats.syntax.functor._

// TODO: add comonad to show where import/export is declared
// Fully resolved Scope must have no expected abilities (all resolved)
case class Names(
  // None means "inherit"
  peerId: Option[Value] = None,
  // Take vars, set vars
  // Data type is not yet known
  importData: Set[String] = Set.empty,
  exportData: Set[String] = Set.empty,
  // Abilities can be imported or set
  expectedAbilities: Set[String] = Set.empty,
  resolvedAbilities: Set[String] = Set.empty,
  // We don't know the types yet
  expectArrows: Set[String] = Set.empty,
  // Set when know
  mode: Names.Mode.Value = Names.Mode.Seq
)

object Names {

  object Mode extends Enumeration {
    val Seq, Par, Xor = Value;
  }

  def funcOps[G[_]: Comonad](ops: NonEmptyList[G[FuncOp[G]]]): Names =
    ops.toList.map(_.extract).map(funcOp[G]).foldLeft(Names()) {
      case (acc, n) if n.mode == Mode.Seq =>
        acc.copy(
          exportData = acc.exportData ++ n.exportData,
          importData = acc.importData ++ (n.importData -- acc.exportData),
          resolvedAbilities =
            if (n.peerId == acc.peerId) acc.resolvedAbilities ++ n.resolvedAbilities else acc.resolvedAbilities,
          expectedAbilities = acc.expectedAbilities ++ (n.expectedAbilities -- acc.resolvedAbilities),
          expectArrows = acc.expectArrows ++ n.expectArrows
        )
      case (acc, n) if n.mode == Mode.Par =>
        acc.copy(
          exportData = acc.exportData ++ n.exportData,
          importData = acc.importData ++ n.importData,
          expectedAbilities = acc.expectedAbilities ++ n.expectedAbilities,
          expectArrows = acc.expectArrows ++ n.expectArrows
        )
      case (acc, n) if n.mode == Mode.Xor =>
        acc.copy(
          importData = acc.importData ++ n.importData,
          expectedAbilities = acc.expectedAbilities ++ n.expectedAbilities,
          expectArrows = acc.expectArrows ++ n.expectArrows
        )
    }

  def funcOp[G[_]: Comonad](op: FuncOp[G]): Names =
    op match {
      case FuncCall(fname, fargs) =>
        Names(
          expectArrows = Set(fname.extract),
          importData = fargs
            .collect(_.extract match {
              case VarLambda(name, _) => name
            })
            .toSet
        )
      case AbilityFuncCall(ab, fc) =>
        val funcNames = funcOp(fc.extract)
        funcNames.copy(expectedAbilities = Set(ab.extract))
      case Extract(n, fn) =>
        val funcNames = funcOp(fn.extract)
        funcNames.copy(exportData = Set(n.extract))
      case AbilityId(ab, _) =>
        Names(resolvedAbilities = Set(ab.extract))
      case On(p, ops) =>
        val ns = funcOps(ops.map(_.widen[FuncOp[G]])).copy(peerId = Some(p.extract))
        p.extract match {
          case VarLambda(name, _) =>
            ns.copy(importData = ns.importData + name)
          case _ => ns
        }
      case Par(op) =>
        funcOp(op.extract).copy(mode = Mode.Par)
    }
}
