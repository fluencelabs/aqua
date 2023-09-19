package aqua.model.inline

import aqua.errors.Errors.internalError
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.model.*
import aqua.model.inline.RawValueInliner.collectionToModel
import aqua.model.inline.raw.CallArrowRawInliner
import aqua.raw.value.ApplyBinaryOpRaw.Op as BinOp
import aqua.raw.ops.*
import aqua.raw.value.*
import aqua.types.{BoxType, CanonStreamType, DataType, StreamType}
import aqua.model.inline.Inline.parDesugarPrefixOpt

import cats.syntax.traverse.*
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.instances.list.*
import cats.data.{Chain, State, StateT}
import cats.syntax.show.*
import cats.syntax.bifunctor.*
import scribe.{log, Logging}

/**
 * [[TagInliner]] prepares a [[RawTag]] for futher processing by converting [[ValueRaw]]s into [[ValueModel]]s.
 *
 * Converts [[ValueRaw]]s into [[ValueModel]]s with [[RawValueInliner]]
 * Inlines [[CallArrowTag]] using [[ArrowInliner]]
 *
 * Doing so might require some tree, expressed as [[OpModel.Tree]], to be prepended before the
 * resulting node. Hence the return types: (model, Option(tree to prepend)).
 */
object TagInliner extends Logging {

  import RawValueInliner.{callToModel, valueListToModel, valueToModel}

  import aqua.model.inline.Inline.parDesugarPrefix

  /**
   * Result of [[RawTag]] inlining
   *
   * @param prefix Previous instructions
   */
  enum TagInlined(prefix: Option[OpModel.Tree]) {

    /**
     * Tag inlining emitted nothing
     */
    case Empty(
      prefix: Option[OpModel.Tree] = None
    ) extends TagInlined(prefix)

    /**
     * Tag inlining emitted one parent model
     *
     * @param model Model which will wrap children
     */
    case Single(
      model: OpModel,
      prefix: Option[OpModel.Tree] = None
    ) extends TagInlined(prefix)

    /**
     * Tag inling emitted complex transformation
     *
     * @param toModel Function from children results to result of this tag
     */
    case Mapping(
      toModel: Chain[OpModel.Tree] => OpModel.Tree,
      prefix: Option[OpModel.Tree] = None
    ) extends TagInlined(prefix)

    /**
     * Finalize inlining, construct a tree
     *
     * @param children Children results
     * @return Result of inlining
     */
    def build(children: Chain[OpModel.Tree]): OpModel.Tree = {
      val inlined = this match {
        case Empty(_) => children
        case Single(model, _) =>
          Chain.one(model.wrap(children))
        case Mapping(toModel, _) =>
          Chain.one(toModel(children))
      }

      SeqModel.wrap(Chain.fromOption(prefix) ++ inlined)
    }
  }

  private def pure[S](op: OpModel): State[S, TagInlined] =
    TagInlined.Single(model = op).pure

  private def none[S]: State[S, TagInlined] =
    TagInlined.Empty().pure

  private def combineOpsWithSeq(l: Option[OpModel.Tree], r: Option[OpModel.Tree]) =
    l match {
      case None => r
      case Some(a) =>
        r match {
          case None => l
          case Some(b) => Some(SeqModel.wrap(a, b))
        }
    }

  def canonicalizeIfStream[S: Mangler](
    vm: ValueModel,
    ops: Option[OpModel.Tree] = None
  ): State[S, (ValueModel, Option[OpModel.Tree])] = {
    vm match {
      case VarModel(name, StreamType(el), l) =>
        val canonName = name + "_canon"
        Mangler[S].findAndForbidName(canonName).map { n =>
          val canon = VarModel(n, CanonStreamType(el), l)
          val canonModel = CanonicalizeModel(
            operand = vm,
            exportTo = CallModel.Export(
              canon.name,
              canon.`type`
            )
          ).leaf
          canon -> combineOpsWithSeq(ops, canonModel.some)
        }
      case _ => State.pure(vm -> ops)
    }
  }

  def flat[S: Mangler](
    vm: ValueModel,
    op: Option[OpModel.Tree],
    flatStream: Boolean
  ): State[S, (ValueModel, Option[OpModel.Tree])] = {
    vm match {
      case v @ VarModel(n, StreamType(t), l) if flatStream =>
        val canonName = n + "_canon"
        for {
          canonN <- Mangler[S].findAndForbidName(canonName)
          canonV = VarModel(canonN, CanonStreamType(t), l)
          canonOp = CanonicalizeModel(
            v.copy(properties = Chain.empty),
            CallModel.Export(canonV.name, canonV.baseType)
          ).leaf
        } yield (canonV, combineOpsWithSeq(op, canonOp.some))
      case _ => (vm, op).pure
    }
  }

  private def flatCanonStream[S: Mangler](
    canonV: VarModel,
    op: Option[OpModel.Tree]
  ): State[S, (ValueModel, Option[OpModel.Tree])] = {
    if (canonV.properties.nonEmpty) {
      val apName = canonV.name + "_flatten"
      Mangler[S].findAndForbidName(apName).map { apN =>
        val apV = VarModel(apN, canonV.`type`)
        val apOp = FlattenModel(canonV, apN).leaf
        (
          apV,
          combineOpsWithSeq(op, apOp.some)
        )
      }
    } else {
      State.pure((canonV, op))
    }

  }

  /**
   * Processes a single [[RawTag]] that may lead to many changes, including calling [[ArrowInliner]]
   *
   * @param tag Tag to process
   * @tparam S Current state
   * @return Model (if any), and prefix (if any)
   */
  def tagToModel[S: Mangler: Arrows: Exports](
    tag: RawTag,
    treeFunctionName: String
  ): State[S, TagInlined] =
    tag match {
      case OnTag(peerId, via, strategy) =>
        for {
          peerIdDe <- valueToModel(peerId)
          viaDe <- valueListToModel(via.toList)
          viaDeFlattened <- viaDe.traverse { case (vm, tree) =>
            flat(vm, tree, true)
          }
          (pid, pif) = peerIdDe
          (viaD, viaF) = viaDeFlattened.unzip
            .bimap(Chain.fromSeq, _.flatten)
          strat = strategy.map { case OnTag.ReturnStrategy.Relay =>
            OnModel.ReturnStrategy.Relay
          }
          toModel = (children: Chain[OpModel.Tree]) =>
            XorModel.wrap(
              OnModel(pid, viaD, strat).wrap(
                children
              ),
              // This will return to previous topology
              // and propagate error up
              FailModel(ValueModel.lastError).leaf
            )
        } yield TagInlined.Mapping(
          toModel = toModel,
          prefix = parDesugarPrefix(viaF.prependedAll(pif))
        )

      case IfTag(valueRaw) =>
        (valueRaw match {
          // Optimize in case last operation is equality check
          case ApplyBinaryOpRaw(op @ (BinOp.Eq | BinOp.Neq), left, right) =>
            (
              valueToModel(left) >>= canonicalizeIfStream,
              valueToModel(right) >>= canonicalizeIfStream
            ).mapN { case ((lmodel, lprefix), (rmodel, rprefix)) =>
              val prefix = parDesugarPrefixOpt(lprefix, rprefix)
              val matchModel = MatchMismatchModel(
                left = lmodel,
                right = rmodel,
                shouldMatch = op match {
                  case BinOp.Eq => true
                  case BinOp.Neq => false
                }
              )

              (prefix, matchModel)
            }
          case _ =>
            valueToModel(valueRaw).map { case (valueModel, prefix) =>
              val matchModel = MatchMismatchModel(
                left = valueModel,
                right = LiteralModel.bool(true),
                shouldMatch = true
              )

              (prefix, matchModel)
            }
        }).map { case (prefix, matchModel) =>
          val toModel = (children: Chain[OpModel.Tree]) =>
            XorModel.wrap(
              children.uncons.map { case (ifBody, elseBody) =>
                val elseBodyFiltered = elseBody.filterNot(
                  _.head == EmptyModel
                )

                /**
                 * Hack for xor with mismatch always have second branch
                 * TODO: Fix this in topology
                 * see https://linear.app/fluence/issue/LNG-69/if-inside-on-produces-invalid-topology
                 */
                val elseBodyAugmented =
                  if (elseBodyFiltered.isEmpty)
                    Chain.one(
                      NullModel.leaf
                    )
                  else elseBodyFiltered

                matchModel.wrap(ifBody) +: elseBodyAugmented
              }.getOrElse(children)
            )

          TagInlined.Mapping(
            toModel = toModel,
            prefix = prefix
          )
        }

      case TryTag => pure(XorModel)

      case ForTag(item, iterable, mode) =>
        for {
          vp <- valueToModel(iterable)
          flattened <- flat(vp._1, vp._2, true)
          (v, p) = flattened
          n <- Mangler[S].findAndForbidName(item)
          elementType = iterable.`type` match {
            case b: BoxType => b.element
            case _ =>
              internalError(
                s"non-box type variable '$iterable' in 'for' expression."
              )
          }
          _ <- Exports[S].resolved(item, VarModel(n, elementType))
          m = mode.map {
            case ForTag.Mode.Wait => ForModel.Mode.Never
            case ForTag.Mode.Pass => ForModel.Mode.Null
          }
        } yield TagInlined.Single(
          model = ForModel(n, v, m),
          prefix = p
        )

      case PushToStreamTag(operand, exportTo) =>
        (
          valueToModel(operand),
          // We need to resolve stream because it could
          // be actually pointing to another var.
          // TODO: Looks like a hack, refator resolving
          valueToModel(exportTo.toRaw)
        ).mapN {
          case ((v, p), (VarModel(name, st, Chain.nil), None)) =>
            TagInlined.Single(
              model = PushToStreamModel(v, CallModel.Export(name, st)),
              prefix = p
            )
          case (_, (vm, prefix)) =>
            internalError(
              s"stream (${exportTo}) resolved " +
                s"to ($vm) with prefix ($prefix)"
            )
        }

      case CanonicalizeTag(operand, exportTo) =>
        valueToModel(operand).flatMap {
          // pass literals as is
          case (l @ LiteralModel(_, _), p) =>
            Exports[S]
              .resolved(exportTo.name, l)
              .as(TagInlined.Empty(prefix = p))
          case (v, p) =>
            Exports[S]
              .resolved(
                exportTo.name,
                VarModel(exportTo.name, exportTo.`type`)
              )
              .as(
                TagInlined.Single(
                  model = CanonicalizeModel(v, CallModel.callExport(exportTo)),
                  prefix = p
                )
              )
        }

      case FlattenTag(operand, assignTo) =>
        valueToModel(operand).flatMap {
          // pass literals as is
          case (l @ LiteralModel(_, _), p) =>
            for {
              _ <- Exports[S].resolved(assignTo, l)
            } yield TagInlined.Empty(prefix = p)
          case (v, p) =>
            TagInlined
              .Single(
                model = FlattenModel(v, assignTo),
                prefix = p
              )
              .pure
        }

      case JoinTag(operands) =>
        operands
          .traverse(o => valueToModel(o))
          .map(nel => {
            logger.trace("join after " + nel.map(_._1))
            // Empty because join behaviour will be processed in ApplyPropertiesRawInliner
            TagInlined.Empty(prefix = parDesugarPrefix(nel.toList.flatMap(_._2)))
          })

      case CallArrowRawTag(exportTo, value: CallArrowRaw) =>
        CallArrowRawInliner.unfoldArrow(value, exportTo).flatMap { case (_, inline) =>
          RawValueInliner
            .inlineToTree(inline)
            .map(tree =>
              TagInlined.Empty(
                prefix = SeqModel.wrap(tree).some
              )
            )
        }

      case AssignmentTag(value, assignTo) =>
        for {
          modelAndPrefix <- value match {
            // if we assign collection to a stream, we must use it's name, because it is already created with 'new'
            case c @ CollectionRaw(_, _: StreamType) =>
              collectionToModel(c, Some(assignTo))
            case v =>
              valueToModel(v, false)
          }
          (model, prefix) = modelAndPrefix
          _ <- Exports[S].resolved(assignTo, model)
        } yield TagInlined.Empty(prefix = prefix)

      case ClosureTag(arrow, detach) =>
        if (detach) Arrows[S].resolved(arrow, None).as(TagInlined.Empty())
        else
          Arrows[S]
            .resolved(arrow, arrow.name.some)
            .as(TagInlined.Single(model = CaptureTopologyModel(arrow.name)))

      case NextTag(item) =>
        for {
          exps <- Exports[S].exports
          model = exps.get(item).collect { case VarModel(n, _, _) =>
            NextModel(n)
          }
        } yield model.fold(TagInlined.Empty())(m => TagInlined.Single(model = m))

      case RestrictionTag(name, typ) =>
        pure(RestrictionModel(name, typ))

      case DeclareStreamTag(value) =>
        value match
          case VarRaw(name, _) =>
            for {
              cd <- valueToModel(value)
              (vm, prefix) = cd
              _ <- Exports[S].resolved(name, vm)
            } yield TagInlined.Empty(prefix = prefix)
          case _ => none

      case ServiceIdTag(id, serviceType, name) =>
        for {
          idm <- valueToModel(id)
          (idModel, idPrefix) = idm

          // Make `FuncArrow` wrappers for service methods
          methods <- serviceType.fields.toSortedMap.toList.traverse {
            case (methodName, methodType) =>
              for {
                arrowName <- Mangler[S].findAndForbidName(s"$name-$methodName")
                fn = FuncArrow.fromServiceMethod(
                  arrowName,
                  serviceType.name,
                  methodName,
                  methodType,
                  idModel
                )
              } yield methodName -> fn
          }

          // Resolve wrappers in arrows
          _ <- Arrows[S].resolved(
            methods.map { case (_, fn) =>
              fn.funcName -> fn
            }.toMap
          )

          // Resolve wrappers in exports
          _ <- methods.traverse { case (methodName, fn) =>
            Exports[S].resolveAbilityField(
              name,
              methodName,
              VarModel(fn.funcName, fn.arrowType)
            )
          }

          // Resolve service in exports
          _ <- Exports[S].resolved(
            name,
            VarModel(name, serviceType)
          )
        } yield TagInlined.Empty(prefix = idPrefix)

      case _: SeqGroupTag => pure(SeqModel)
      case ParTag.Detach => pure(DetachModel)
      case _: ParGroupTag => pure(ParModel)

      case _: NoExecTag => none
      case _ =>
        logger.warn(s"Tag $tag must have been eliminated at this point")
        none
    }

  private def traverseS[S](
    cf: RawTag.Tree,
    f: RawTag => State[S, TagInlined]
  ): State[S, OpModel.Tree] =
    for {
      headInlined <- f(cf.head)
      tail <- StateT.liftF(cf.tail)
      children <- tail.traverse(traverseS[S](_, f))
    } yield headInlined.build(children)

  def handleTree[S: Exports: Mangler: Arrows](
    tree: RawTag.Tree,
    treeFunctionName: String
  ): State[S, OpModel.Tree] =
    traverseS(tree, tagToModel(_, treeFunctionName))
}
