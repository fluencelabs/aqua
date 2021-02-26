package aqua

import aqua.parser.lexer.{Value, VarLambda}
import aqua.parser._
import cats.data.Validated.Valid
import cats.{Comonad, Functor}
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.syntax.comonad._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.validated._

// Fully resolved Scope must have no expected abilities (all resolved)
case class Names[F[_]](
  // None means "inherit"
  peerId: Option[F[Value]] = None,
  // Take vars, set vars
  // Data type is not yet known
  importData: Names.Acc[F, Value] = Names.Acc.empty[F, Value],
  exportData: Names.Acc[F, String] = Names.Acc.empty[F, String],
  // Abilities can be imported or set
  unresolvedAbilities: Names.Acc[F, String] = Names.Acc.empty[F, String],
  resolvedAbilities: Names.Acc[F, String] = Names.Acc.empty[F, String],
  // Abilities can be defined and expected
  expectedAbilities: Names.Acc[F, String] = Names.Acc.empty[F, String],
  definedAbilities: Names.Acc[F, DefService[F]] = Names.Acc.empty[F, DefService[F]],
  // Types can be defined and expected
  expectedTypes: Names.Acc[F, CustomType] = Names.Acc.empty[F, CustomType],
  definedTypes: Names.Acc[F, Option[DefType[F]]] = Names.Acc.empty[F, Option[DefType[F]]],
  // We don't know the types yet
  expectedArrows: Names.Acc[F, String] = Names.Acc.empty[F, String],
  definedArrows: Names.Acc[F, String] = Names.Acc.empty[F, String],
  // Set when know
  mode: Option[F[Names.CustomMode]] = None
)

object Names {

  case class Acc[F[_], T](data: Map[String, NonEmptyList[F[T]]]) {

    def add(other: Acc[F, T], subtract: Set[String] = Set.empty): Acc[F, T] =
      copy(data = (other.data -- subtract).foldLeft(data) {
        case (accD, (k, v)) =>
          accD.updatedWith(k)(dv => Option(dv.fold(v)(_ ++ v.toList)))
      })

    def keys = data.keySet

    def sub(n: String) = copy(data = data - n)

    def toErrors(toMsg: (String, F[T]) => String)(implicit F: Functor[F]): List[F[String]] =
      data.flatMap {
        case (n, vs) => vs.toList.map(ft => ft.as(toMsg(n, ft)))
      }.toList

    def erase: Acc[F, T] = Acc.empty

    def addOne(n: String, v: F[T]) = add(Acc.one(n, v))
  }

  object Acc {
    def empty[F[_], T]: Acc[F, T] = Acc(Map.empty[String, NonEmptyList[F[T]]])

    def str[F[_]: Comonad](v: F[String]): Acc[F, String] = one(v.extract, v)

    def one[F[_], T](n: String, v: F[T]): Acc[F, T] = Acc(Map(n -> NonEmptyList.one(v)))
  }

  sealed trait CustomMode
  case object ParMode extends CustomMode
  case object XorMode extends CustomMode

  private def combineSeq[G[_]: Comonad](a: Names[G], b: Names[G]): Names[G] =
    a.copy(
      exportData = a.exportData add b.exportData,
      importData = a.importData.add(b.importData, a.exportData.keys),
      resolvedAbilities =
        if (b.peerId == a.peerId) a.resolvedAbilities add b.resolvedAbilities else a.resolvedAbilities,
      unresolvedAbilities = a.unresolvedAbilities.add(b.unresolvedAbilities, a.resolvedAbilities.keys),
      expectedArrows = a.expectedArrows.add(b.expectedArrows, a.definedArrows.keys),
      definedArrows = a.definedArrows add b.definedArrows,
      expectedTypes = a.expectedTypes.add(b.expectedTypes, a.definedTypes.keys),
      definedTypes = a.definedTypes add b.definedTypes,
      expectedAbilities = a.expectedAbilities.add(b.expectedAbilities, a.definedAbilities.keys),
      definedAbilities = a.definedAbilities add b.definedAbilities
    )

  def funcOps[G[_]: Comonad](ops: NonEmptyList[G[FuncOp[G]]]): Names[G] =
    ops.map(funcOp[G]).foldLeft[Names[G]](Names[G]()) {
      case (acc, n) if n.mode.isEmpty =>
        combineSeq(acc, n)
      case (acc, n) if n.mode.exists(_.extract == ParMode) =>
        acc.copy(
          exportData = acc.exportData add n.exportData,
          importData = acc.importData add n.importData,
          unresolvedAbilities = acc.unresolvedAbilities add n.unresolvedAbilities,
          expectedArrows = acc.expectedArrows add n.expectedArrows,
          expectedTypes = acc.expectedTypes add n.expectedTypes,
          expectedAbilities = acc.expectedAbilities add n.expectedAbilities
        )
      case (acc, n) if n.mode.exists(_.extract == XorMode) =>
        acc.copy(
          importData = acc.importData add n.importData,
          unresolvedAbilities = acc.unresolvedAbilities add n.unresolvedAbilities,
          expectedArrows = acc.expectedArrows add n.expectedArrows,
          expectedTypes = acc.expectedTypes add n.expectedTypes,
          expectedAbilities = acc.expectedAbilities add n.expectedAbilities
        )
    }

  private def valuesToNames[G[_]: Comonad](args: List[G[Value]]): Acc[G, Value] =
    args
      .collect(arg =>
        arg.extract match {
          case VarLambda(name, _) => Acc.one[G, Value](name, arg)
        }
      )
      .foldLeft(Acc.empty[G, Value])(_ add _)

  def funcOp[G[_]: Comonad](op: G[FuncOp[G]]): Names[G] =
    op.extract match {
      case FuncCall(fname, fargs) =>
        Names[G](
          expectedArrows = Acc.str(fname),
          importData = valuesToNames(fargs)
        )
      case AbilityFuncCall(ab, fc) =>
        val funcNames = funcOp(fc.widen[FuncOp[G]])
        funcNames.copy(unresolvedAbilities = Acc.one(ab.extract, ab), expectedArrows = Acc.empty)
      case Extract(n, fn) =>
        val funcNames = funcOp(fn.widen[FuncOp[G]])
        funcNames.copy(exportData = Acc.str(n))
      case AbilityId(ab, id) =>
        Names[G](
          resolvedAbilities = Acc.str(ab),
          expectedAbilities = Acc.str(ab),
          importData = valuesToNames(id :: Nil)
        )
      case On(p, ops) =>
        val ns = funcOps(ops.map(_.widen[FuncOp[G]])).copy(peerId = Some(p))
        p.extract match {
          case VarLambda(name, _) =>
            ns.copy(importData = ns.importData add Acc.one(name, p))
          case _ => ns
        }
      case Par(op) =>
        funcOp(op.widen[FuncOp[G]]).copy(mode = Some(op.as(ParMode)))
    }

  def typeAcc[G[_]: Comonad](t: G[Type]): Acc[G, CustomType] =
    t.extract match {
      case ct: CustomType =>
        Acc.one(ct.name, t.as(ct))
      case at: ArrayType =>
        typeAcc(t.as(at.data))
      case at: ArrowType =>
        (t.as[Type](at.res) :: at.args.map(t.as[Type](_)))
          .map[Acc[G, CustomType]](v => typeAcc[G](v))
          .foldLeft[Acc[G, CustomType]](Acc.empty[G, CustomType])(_ add _)
      case _: BasicType =>
        Acc.empty
    }

  def funcHeadNames[G[_]: Comonad](head: FuncHead[G], body: Names[G]): Names[G] =
    head.args.foldLeft(
      body.copy(
        // We clear the mode, as functions are always defined in a sequence
        mode = None,
        // Function may have result type, but it's not names
        exportData = body.exportData.erase,
        // Until we have a notion for exporting abilities, they're cleaned
        resolvedAbilities = body.resolvedAbilities.erase,
        // Create arrow from this func
        definedArrows = Acc.str(head.name),
        // Expected arrows are not reduced by this func's name, hence recursive functions are forbidden
        expectedTypes = head.args.map(_._2._2).map(typeAcc(_)).foldLeft(body.expectedTypes)(_ add _),
        // Even if peer is defined, it's defined inside
        peerId = None
      )
    ) {
      case (names, (k, (_, ft))) =>
        ft.extract match {
          case cd: CustomType =>
            names.copy(
              importData = names.importData sub k,
              expectedTypes = names.expectedTypes.addOne(cd.name, ft.as(cd))
            )
          case cd: ArrayType =>
            names.copy(
              importData = names.importData sub k,
              expectedTypes = names.expectedTypes add typeAcc[G](ft.as(cd.data))
            )
          case _: ArrowType =>
            names.copy(expectedArrows = names.expectedArrows sub k)
        }
    }

  def funcNames[G[_]: Comonad](func: DefFunc[G]): Names[G] =
    funcHeadNames(func.head, funcOps(func.body))

  def blockNames[G[_]: Comonad](block: Block[G]): Names[G] =
    block match {
      case func: DefFunc[G] =>
        funcNames(func)
      case deft: DefType[G] =>
        Names[G](
          definedTypes = Acc.one(deft.name.extract, deft.name.as(Some(deft))),
          expectedTypes = deft.fields.toNel.map {
            case (_, (_, tv)) =>
              typeAcc(tv.widen[Type])
          }.foldLeft(Acc.empty[G, CustomType])(_ add _)
        )
      case alias: DefAlias[G] =>
        Names[G](
          definedTypes =
            Acc.one[G, Option[DefType[G]]](alias.alias.extract.name, alias.target.as(Option.empty[DefType[G]])),
          expectedTypes = typeAcc(alias.target)
        )
      case _ =>
        // Until we care about types, there's no imports/exports
        Names[G]()
    }

  def foldVerify[G[_]: Comonad](input: List[Names[G]]): ValidatedNel[G[String], Names[G]] =
    input.foldLeft[ValidatedNel[G[String], Names[G]]](Valid(Names[G]())) {
      case (accE, ns) =>
        accE.map(acc => combineSeq[G](acc, ns)).andThen { acc =>
          Validated.fromEither[NonEmptyList[G[String]], Names[G]](
            NonEmptyList
              .fromList(
                acc.importData.toErrors((v, _) => s"Unknown variable `${v}`") :::
                  acc.expectedArrows.toErrors((a, _) => s"Unknown arrow `${a}`") :::
                  acc.unresolvedAbilities.toErrors((a, _) => s"Unresolved ability `${a}`") :::
                  acc.expectedAbilities.toErrors((a, _) => s"Undefined ability `${a}`") :::
                  acc.expectedTypes.toErrors((t, _) => s"Undefined type `$t`")
              )
              .toLeft(acc)
          )

        }
    }
}
