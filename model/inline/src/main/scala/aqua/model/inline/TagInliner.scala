package aqua.model.inline

import aqua.errors.Errors.internalError
import aqua.model.*
import aqua.model.inline.raw.{CallArrowRawInliner, CallServiceRawInliner}
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.model.inline.tag.IfTagInliner
import aqua.raw.ops.*
import aqua.raw.value.*
import aqua.types.{CanonStreamType, CollectionType, StreamType}

import cats.data.{Chain, State, StateT}
import cats.instances.list.*
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.bifunctor.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import scribe.Logging

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

  import aqua.model.inline.Inline.parDesugarPrefix

  import RawValueInliner.{valueListToModel, valueToModel}

  /**
   * Result of [[RawTag]] inlining
   *
   * @param prefix Previous instructions
   */
  enum TagInlined[T](prefix: Option[OpModel.Tree]) {

    /**
     * Tag inlining emitted nothing
     */
    case Empty[S](
      prefix: Option[OpModel.Tree] = None
    ) extends TagInlined[S](prefix)

    /**
     * Tag inlining emitted one parent model
     *
     * @param model Model which will wrap children
     */
    case Single[S](
      model: OpModel,
      prefix: Option[OpModel.Tree] = None
    ) extends TagInlined[S](prefix)

    /**
     * Tag inling emitted complex transformation
     *
     * @param toModel Function from children results to result of this tag
     */
    case Mapping[S](
      toModel: Chain[OpModel.Tree] => OpModel.Tree,
      prefix: Option[OpModel.Tree] = None
    ) extends TagInlined[S](prefix)

    /**
     * Tag inlining emitted computation
     * that should be executed after children
     *
     * @param model computation producing model
     */
    case After[S](
      model: State[S, OpModel],
      prefix: Option[OpModel.Tree] = None
    ) extends TagInlined[S](prefix)

    /**
     * Finalize inlining, construct a tree
     *
     * @param children Children results
     * @return Result of inlining
     */
    def build(children: Chain[OpModel.Tree]): State[T, OpModel.Tree] = {
      def toSeqModel(tree: OpModel.Tree | Chain[OpModel.Tree]): State[T, OpModel.Tree] = {
        val treeChain = tree match {
          case c: Chain[OpModel.Tree] => c
          case t: OpModel.Tree => Chain.one(t)
        }

        State.pure(SeqModel.wrap(Chain.fromOption(prefix) ++ treeChain))
      }

      this match {
        case Empty(_) =>
          toSeqModel(children)
        case Single(model, _) =>
          toSeqModel(model.wrap(children))
        case Mapping(toModel, _) =>
          toSeqModel(toModel(children))
        case After(model, _) =>
          model.flatMap(m => toSeqModel(m.wrap(children)))
      }

    }
  }

  private def pure[S](op: OpModel): State[S, TagInlined[S]] =
    TagInlined.Single(model = op).pure

  private def none[S]: State[S, TagInlined[S]] =
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
    tag: RawTag
  ): State[S, TagInlined[S]] =
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
              FailModel(ValueModel.error).leaf
            )
        } yield TagInlined.Mapping(
          toModel = toModel,
          prefix = parDesugarPrefix(viaF.prependedAll(pif))
        )

      case IfTag(valueRaw) =>
        IfTagInliner(valueRaw).inlined.map(inlined =>
          TagInlined.Mapping(
            toModel = inlined.toModel,
            prefix = inlined.prefix
          )
        )

      case TryTag => pure(XorModel)

      case ForTag(item, iterable, mode) =>
        for {
          vp <- valueToModel(iterable)
          flattened <- flat(vp._1, vp._2, true)
          (v, p) = flattened
          n <- Mangler[S].findAndForbidName(item)
          elementType = iterable.`type` match {
            case b: CollectionType => b.element
            case _ =>
              internalError(
                s"non-box type variable '$iterable' in 'for' expression."
              )
          }
          _ <- Exports[S].resolved(item, VarModel(n, elementType))
          modeModel = mode match {
            case ForTag.Mode.Blocking => ForModel.Mode.Never
            case ForTag.Mode.NonBlocking => ForModel.Mode.Null
          }
        } yield TagInlined.Single(
          model = ForModel(n, v, modeModel),
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
              s"stream ($exportTo) resolved " +
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

      case CallArrowRawTag(exportTo, value: (CallArrowRaw | CallServiceRaw)) =>
        (value match {
          case ca: CallArrowRaw =>
            CallArrowRawInliner.unfold(ca, exportTo)
          case cs: CallServiceRaw =>
            CallServiceRawInliner.unfold(cs, exportTo)
        }).flatMap { case (_, inline) =>
          RawValueInliner
            .inlineToTree(inline)
            .map(tree =>
              TagInlined.Empty(
                prefix = SeqModel.wrap(tree).some
              )
            )
        }

      case CallArrowRawTag(
            exportTo,
            ApplyPropertyRaw(vr, IntoArrowRaw(name, at, args))
          ) =>
        RawValueInliner.valueToModel(vr).flatMap {
          // the name of VarModel was already converted to abilities full name
          case (VarModel(n, _, _), prevInline) =>
            CallArrowRawInliner.unfold(CallArrowRaw.ability(n, name, at, args), exportTo).flatMap {
              case (_, inline) =>
                RawValueInliner
                  .inlineToTree(inline.prepend(prevInline))
                  .map(tree =>
                    TagInlined.Empty(
                      prefix = SeqModel.wrap(tree).some
                    )
                  )
            }
          case _ =>
            internalError(s"Unexpected. 'IntoArrowRaw' can be only used on variables")

        }
      case CallArrowRawTag(_, value) =>
        internalError(s"Cannot inline 'CallArrowRawTag' with value '$value'")

      case AssignmentTag(value, assignTo) =>
        for {
          modelAndPrefix <- valueToModel(value, false)
          (model, prefix) = modelAndPrefix
          _ <- Exports[S].resolved(assignTo, model)
        } yield TagInlined.Empty(prefix = prefix)

      case ClosureTag(arrow, detach) =>
        if (detach) Arrows[S].resolved(arrow, None, true).as(TagInlined.Empty())
        else
          Arrows[S]
            .resolved(arrow, arrow.name.some, true)
            .as(TagInlined.Single(model = CaptureTopologyModel(arrow.name)))

      case NextTag(item) =>
        for {
          exps <- Exports[S].exports
          model = exps.get(item).collect { case VarModel(n, _, _) =>
            NextModel(n)
          }
        } yield model.fold(TagInlined.Empty())(m => TagInlined.Single(model = m))

      case RestrictionTag(name, typ) =>
        // Rename restriction after children are inlined with new exports
        TagInlined
          .After(
            for {
              exps <- Exports[S].exports
              model = exps.get(name).collect { case VarModel(n, _, _) =>
                RestrictionModel(n, typ)
              }
            } yield model.getOrElse(RestrictionModel(name, typ))
          )
          .pure

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
    f: RawTag => State[S, TagInlined[S]]
  ): State[S, OpModel.Tree] =
    for {
      headInlined <- f(cf.head)
      tail <- StateT.liftF(cf.tail)
      children <- tail.traverse(traverseS[S](_, f))
      inlined <- headInlined.build(children)
    } yield inlined

  def handleTree[S: Exports: Mangler: Arrows](
    tree: RawTag.Tree
  ): State[S, OpModel.Tree] =
    traverseS(tree, tagToModel(_))
}
