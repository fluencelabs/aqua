package aqua.model.inline.raw

import aqua.model.{CallModel, CallServiceModel, CanonicalizeModel, FlattenModel, ForModel, FunctorModel, IntoIndexModel, LambdaModel, LiteralModel, MatchMismatchModel, NextModel, PushToStreamModel, RestrictionModel, SeqModel, ValueModel, VarModel, XorModel}
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.unfold
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.value.{ApplyLambdaRaw, CallArrowRaw, FunctorRaw, IntoIndexRaw, LambdaRaw, LiteralRaw, ValueRaw, VarRaw}
import aqua.types.{ArrayType, CanonStreamType, ScalarType, StreamType}
import cats.data.{Chain, State}
import cats.syntax.monoid.*
import cats.instances.list.*

object ApplyLambdaRawInliner extends RawInliner[ApplyLambdaRaw] {

  private[inline] def removeLambda[S: Mangler: Exports: Arrows](
    vm: ValueModel
  ): State[S, (ValueModel, Inline)] =
    vm match {
      case VarModel(nameM, btm, lambdaM) if lambdaM.nonEmpty =>
        for {
          nameMM <- Mangler[S].findAndForbidName(nameM)
        } yield VarModel(nameMM, vm.`type`, Chain.empty) -> Inline.preload(
          // TODO use smth more resilient to make VarRaw from a flattened VarModel
          nameMM -> ApplyLambdaRaw.fromChain(VarRaw(nameM, btm), lambdaM.map(_.toRaw))
        )
      case _ =>
        State.pure(vm -> Inline.empty)
    }

  private def splitFunctors[S: Mangler: Exports: Arrows](
    vm: ValueRaw,
    lambdaM: Chain[LambdaRaw]
  ): State[S, (ValueModel, Inline)] = {
    println("split: " + vm)
    vm match {
      case vrm@VarRaw(nameM, btm) if lambdaM.nonEmpty =>
        val lambdas = lambdaM.toList
        val firstFunctorIdx = lambdas.indexWhere {
          case FunctorModel(_, _, isField) if !isField =>
            true
          case _ => false
        }

        if (firstFunctorIdx != -1) {
          val (ll, lr) = lambdas.splitAt(firstFunctorIdx)
          val withoutFunctorL = ll.dropRight(1)
          val functor = ll.last
          if (withoutFunctorL.nonEmpty) {
            for {
              nameLeftLambda <- Mangler[S].findAndForbidName(nameM)
              nameFunctor <- Mangler[S].findAndForbidName(nameM)
              functorSide <- unfoldLambda(functor)
              rightSide <- splitFunctors(VarRaw(nameFunctor, btm), Chain.fromSeq(lr))
              leftSide <- unfold(VarRaw(vrm.name, vrm.baseType).withLambda(withoutFunctorL: _*))
            } yield {
              val (leftVM, leftInline) = leftSide
              val (functorL, functorInline) = functorSide
              val (rightVM, rightInline) = rightSide
              rightVM -> (leftInline |+| functorInline |+| Inline.tree(
                SeqModel.wrap(
                  FlattenModel(leftVM, nameLeftLambda).leaf,
                  FlattenModel(VarModel(nameLeftLambda, btm, Chain.one(functorL)), nameFunctor).leaf
                )
              ) |+| rightInline)
            }

          } else {
            for {
              nameFunctor <- Mangler[S].findAndForbidName(nameM)
              rightSide <- splitFunctors(VarModel(nameFunctor, btm, Chain.fromSeq(lr)))
              functorSide <- unfoldLambda(functor)
            } yield {
              val (rightVM, rightInline) = rightSide
              val (functorL, functorInline) = functorSide
              rightVM -> (functorInline |+| Inline.tree(
                SeqModel.wrap(
                  FlattenModel(vrm.copy(lambda = Chain.one(functorL)), nameFunctor).leaf
                )
              ) |+| rightInline)
            }
          }
        } else {
          State.pure(vm -> Inline.empty)
        }
      case _ =>
        State.pure(vm -> Inline.empty)
    }
  }

  private[inline] def unfoldLambda[S: Mangler: Exports: Arrows](
    l: LambdaRaw
  ): State[S, (LambdaModel, Inline)] = // TODO lambda for collection
    l match {
      case FunctorRaw(field, t, isField) =>
        State.pure(FunctorModel(field, t, isField) -> Inline.empty)
      case IntoIndexRaw(vm: ApplyLambdaRaw, t) =>
        for {
          nn <- Mangler[S].findAndForbidName("ap-lambda")
        } yield IntoIndexModel(nn, t) -> Inline.preload(nn -> vm)

      case IntoIndexRaw(vr: (VarRaw | CallArrowRaw), t) =>
        unfold(vr, lambdaAllowed = false).map {
          case (VarModel(name, _, _), inline) => IntoIndexModel(name, t) -> inline
          case (LiteralModel(v, _), inline) => IntoIndexModel(v, t) -> inline
        }

      case IntoIndexRaw(LiteralRaw(value, _), t) =>
        State.pure(IntoIndexModel(value, t) -> Inline.empty)
    }

  private def increment(v: ValueModel, result: VarModel) =
    CallServiceModel(
      LiteralModel("\"math\"", ScalarType.string),
      "add",
      CallModel(
        v :: LiteralModel.fromRaw(LiteralRaw.number(1)) :: Nil,
        CallModel.Export(result.name, result.`type`) :: Nil
      )
    ).leaf

  override def apply[S: Mangler: Exports: Arrows](
    alr: ApplyLambdaRaw,
    lambdaAllowed: Boolean
  ): State[S, (ValueModel, Inline)] = Exports[S].exports.flatMap { exports =>
    val (raw, lambda) = alr.unwind
    println("alr: " + alr)
    lambda
      .foldLeft[State[S, (Chain[LambdaModel], Inline)]](
        State.pure(Chain.empty[LambdaModel] -> Inline.empty)
      ) { case (lcm, l) =>
        lcm.flatMap { case (lc, m) =>
          unfoldLambda(l).map { case (lm, mm) =>
            (lc :+ lm, m |+| mm)
          }
        }
      }
      .flatMap { case (lambdaModel, map) =>
        println("raw: " + raw)
        unfold(raw, lambdaAllowed)
          .flatMap {
          case (v: VarModel, prefix) =>
            ((v.`type`, lambdaModel.headOption) match {
              // canonicalize stream
              case (st: StreamType, Some(idx @ IntoIndexModel(_, _))) =>
                val resultName = v.name + "_result_canon"
                Mangler[S].findAndForbidName(resultName).map { uniqueResultName =>
                  val varSTest = VarModel(v.name + "_test", st)
                  val iter = VarModel("s", st.element)

                  val iterCanon = VarModel(v.name + "_iter_canon", CanonStreamType(st.element))

                  val resultCanon =
                    VarModel(uniqueResultName, CanonStreamType(st.element), lambdaModel)

                  val incrVar = VarModel("incr_idx", ScalarType.i64)

                  val tree = RestrictionModel(varSTest.name, true).wrap(
                    ForModel(iter.name, v).wrap(
                      increment(idx.idxToValueModel, incrVar),
                      PushToStreamModel(
                        iter,
                        CallModel.Export(varSTest.name, varSTest.`type`)
                      ).leaf,
                      CanonicalizeModel(
                        varSTest,
                        CallModel.Export(iterCanon.name, iterCanon.`type`)
                      ).leaf,
                      XorModel.wrap(
                        MatchMismatchModel(
                          iterCanon.copy(lambda =
                            Chain.one(FunctorModel("length", ScalarType.`u32`, false))
                          ),
                          incrVar,
                          true
                        ).leaf,
                        NextModel(iter.name).leaf
                      )
                    ),
                    CanonicalizeModel(
                      varSTest,
                      CallModel.Export(resultCanon.name, CanonStreamType(st.element))
                    ).leaf
                  )

                  resultCanon -> Inline.tree(tree)
                }

              case _ =>
                val vm = v.copy(lambda = v.lambda ++ lambdaModel).resolveWith(exports)
                State.pure(vm -> Inline.empty)
            }).flatMap { case (genV, genInline) =>
              if (lambdaAllowed) State.pure(genV -> (prefix |+| map |+| genInline))
              else
                removeLambda(genV).map { case (vmm, mpp) =>
                  vmm -> (prefix |+| mpp |+| map |+| genInline)
                }
            }

          case (v, prefix) =>
            // What does it mean actually? I've no ides
            State.pure((v, prefix |+| map))
        }

      }
  }
}
