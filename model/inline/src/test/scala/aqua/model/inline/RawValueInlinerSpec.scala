package aqua.model.inline

import aqua.mangler.ManglerState
import aqua.model.*
import aqua.model.inline.raw.StreamGateInliner
import aqua.model.inline.state.InliningState
import aqua.raw.value.*
import aqua.types.*
import cats.Eval
import cats.data.{Chain, NonEmptyMap}
import cats.free.Cofree
import cats.syntax.foldable.*
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.SortedMap
import scala.math

class RawValueInlinerSpec extends AnyFlatSpec with Matchers with Inside {

  import RawValueInliner.valueToModel

  def toMangler(noNames: Set[String]) = ManglerState(noNames.map(_ -> 0).toMap)

  def join(stream: VarModel, size: ValueModel) =
    stream match {
      case VarModel(
            streamName,
            streamType: StreamType,
            Chain.`nil`
          ) =>
        StreamGateInliner.joinStreamOnIndexModel(
          streamName = streamName,
          streamType = streamType,
          sizeModel = size,
          testName = streamName + "_test",
          iterName = streamName + "_fold_var",
          canonName = streamName + "_result_canon",
          iterCanonName = streamName + "_iter_canon",
          resultName = streamName + "_gate"
        )
      case _ => ???
    }

  private def numVarWithLength(name: String) =
    VarRaw(name, ArrayType(ScalarType.u32)).withProperty(
      FunctorRaw("length", ScalarType.u32)
    )

  private def index(n: Int) =
    LiteralRaw.number(n)

  private def ysVarRaw(into: Int, name: String = "ys") =
    VarRaw(name, ArrayType(ScalarType.i8)).withProperty(
      IntoIndexRaw(LiteralRaw.number(into), ScalarType.i8)
    )

  private val `raw x[y]` = VarRaw("x", ArrayType(ScalarType.string)).withProperty(
    IntoIndexRaw(
      VarRaw("y", ScalarType.i8),
      ScalarType.string
    )
  )

  private val bType =
    StructType("objectType", NonEmptyMap(("c", ScalarType.string), SortedMap.empty))

  private val aType = StructType(
    "objectType",
    NonEmptyMap(
      ("b", bType),
      SortedMap.empty
    )
  )

  private val `raw res.c` = VarRaw(
    "res",
    bType
  ).withProperty(
    FunctorRaw(
      "c",
      ScalarType.string
    )
  )

  private val `raw ys[0]` = IntoIndexRaw(
    ysVarRaw(0),
    ScalarType.string
  )

  private val `raw ys[xyz!]` = IntoIndexRaw(
    ysVarRaw(0, "xyz"),
    ScalarType.string
  )

  private val `raw x[ys[0]]` = VarRaw("x", ArrayType(ScalarType.string)).withProperty(`raw ys[0]`)

  private val `x[xs[ys.length]][xss[yss.length]]` =
    VarRaw("x", ArrayType(ArrayType(ScalarType.string))).withProperty(
      IntoIndexRaw(
        VarRaw("xs", ArrayType(ScalarType.u32))
          .withProperty(
            IntoIndexRaw(
              numVarWithLength("ys"),
              ScalarType.u32
            )
          ),
        ArrayType(ScalarType.string)
      ),
      IntoIndexRaw(
        VarRaw("xss", ArrayType(ScalarType.u32))
          .withProperty(
            IntoIndexRaw(
              numVarWithLength("yss"),
              ScalarType.u32
            )
          ),
        ScalarType.string
      )
    )

  private val `raw x[ys[0]][ys[1]]` =
    VarRaw("x", ArrayType(ArrayType(ScalarType.string))).withProperty(
      IntoIndexRaw(ysVarRaw(0), ArrayType(ScalarType.string)),
      IntoIndexRaw(ysVarRaw(1), ScalarType.string)
    )

  private val `raw x[zs[ys[0]]][ys[1]]` =
    VarRaw("x", ArrayType(ArrayType(ScalarType.string))).withProperty(
      IntoIndexRaw(
        VarRaw("zs", ArrayType(ScalarType.i8)).withProperty(
          IntoIndexRaw(
            ysVarRaw(0),
            ScalarType.i8
          )
        ),
        ArrayType(ScalarType.string)
      ),
      IntoIndexRaw(ysVarRaw(1), ScalarType.string)
    )

  def int(i: Int): LiteralRaw = LiteralRaw.number(i)

  extension (l: ValueRaw) {

    def cmp(op: ApplyBinaryOpRaw.Op.Cmp)(r: ValueRaw): ApplyBinaryOpRaw =
      ApplyBinaryOpRaw(op, l, r, ScalarType.bool)

    def math(op: ApplyBinaryOpRaw.Op.Math)(r: ValueRaw): ApplyBinaryOpRaw =
      ApplyBinaryOpRaw(op, l, r, ScalarType.i64) // result type is not important here

    def `<`(r: ValueRaw): ApplyBinaryOpRaw =
      cmp(ApplyBinaryOpRaw.Op.Lt)(r)

    def `<=`(r: ValueRaw): ApplyBinaryOpRaw =
      cmp(ApplyBinaryOpRaw.Op.Lte)(r)

    def `>`(r: ValueRaw): ApplyBinaryOpRaw =
      cmp(ApplyBinaryOpRaw.Op.Gt)(r)

    def `>=`(r: ValueRaw): ApplyBinaryOpRaw =
      cmp(ApplyBinaryOpRaw.Op.Gte)(r)

    def `+`(r: ValueRaw): ApplyBinaryOpRaw =
      math(ApplyBinaryOpRaw.Op.Add)(r)

    def `-`(r: ValueRaw): ApplyBinaryOpRaw =
      math(ApplyBinaryOpRaw.Op.Sub)(r)

    def `*`(r: ValueRaw): ApplyBinaryOpRaw =
      math(ApplyBinaryOpRaw.Op.Mul)(r)

    def `/`(r: ValueRaw): ApplyBinaryOpRaw =
      math(ApplyBinaryOpRaw.Op.Div)(r)

    def `%`(r: ValueRaw): ApplyBinaryOpRaw =
      math(ApplyBinaryOpRaw.Op.Rem)(r)

    def `**`(r: ValueRaw): ApplyBinaryOpRaw =
      math(ApplyBinaryOpRaw.Op.Pow)(r)

  }

  private def ivar(name: String, t: Option[Type] = None): VarRaw =
    VarRaw(name, t.getOrElse(ScalarType.i32))

  "raw value inliner" should "desugarize a single non-recursive raw value" in {
    // x[y]
    valueToModel[InliningState](`raw x[y]`)
      .runA(InliningState(noNames = toMangler(Set("x", "y"))))
      .value shouldBe (
      VarModel(
        "x",
        ArrayType(ScalarType.string),
        Chain.one(IntoIndexModel("y", ScalarType.string))
      ) -> None
    )
  }

  // TODO: unignore and fix after stream restrictions will be implemented
  ignore /*"raw value inliner"*/ should "unfold an IntoField PropertyModel" in {
    // a.field1.field2
    valueToModel[InliningState](`raw res.c`)
      .runA(
        InliningState(resolvedExports =
          Map("res" -> VarModel("a", aType, Chain.one(IntoFieldModel("b", bType))))
        )
      )
      .value shouldBe (
      VarModel(
        "a",
        aType,
        Chain(IntoFieldModel("b", bType), IntoFieldModel("c", ScalarType.string))
      ) -> None
    )
  }

  it should "desugarize a single recursive raw value" in {
    // x[ys!]
    val (resVal, resTree) = valueToModel[InliningState](
      `raw x[ys[0]]`
    )
      .runA(InliningState(noNames = toMangler(Set("x", "ys"))))
      .value

    resVal should be(
      VarModel(
        "x",
        ArrayType(ScalarType.string),
        Chain.one(IntoIndexModel("ys_flat", ScalarType.string))
      )
    )

    resTree.isEmpty should be(false)

    resTree.get.equalsOrShowDiff(
      FlattenModel(
        VarModel(
          "ys",
          ArrayType(ScalarType.i8),
          Chain.one(IntoIndexModel("0", ScalarType.i8))
        ),
        "ys_flat"
      ).leaf
    ) should be(true)
  }

  it should "desugarize properties with functors x[ys[ys.length]][2] and make proper flattener tags" in {
    val (resVal, resTree) = valueToModel[InliningState](
      `x[xs[ys.length]][xss[yss.length]]`
    ).runA(
      InliningState(noNames = toMangler(Set("x", "ys", "xs", "yss", "xss")))
    ).value

    resVal should be(
      VarModel(
        "x",
        ArrayType(ArrayType(ScalarType.string)),
        Chain(
          IntoIndexModel("xs_flat", ArrayType(ScalarType.string)),
          IntoIndexModel("xss_flat", ScalarType.string)
        )
      )
    )

    resTree.isEmpty should be(false)

    resTree.get.equalsOrShowDiff(
      ParModel.wrap(
        SeqModel.wrap(
          FlattenModel(
            VarModel(
              "ys",
              ArrayType(ScalarType.u32)
            ),
            "ys_to_functor"
          ).leaf,
          FlattenModel(
            VarModel(
              "ys_to_functor",
              ArrayType(ScalarType.u32),
              Chain.one(FunctorModel("length", ScalarType.u32))
            ),
            "ys_length"
          ).leaf
        ),
        FlattenModel(
          VarModel(
            "xs",
            ArrayType(ScalarType.u32),
            Chain.one(IntoIndexModel("ys_length", ScalarType.u32))
          ),
          "xs_flat"
        ).leaf,
        SeqModel.wrap(
          FlattenModel(
            VarModel(
              "yss",
              ArrayType(ScalarType.u32)
            ),
            "yss_to_functor"
          ).leaf,
          FlattenModel(
            VarModel(
              "yss_to_functor",
              ArrayType(ScalarType.u32),
              Chain.one(FunctorModel("length", ScalarType.u32))
            ),
            "yss_length"
          ).leaf
        ),
        FlattenModel(
          VarModel(
            "xss",
            ArrayType(ScalarType.u32),
            Chain.one(IntoIndexModel("yss_length", ScalarType.u32))
          ),
          "xss_flat"
        ).leaf
      )
    ) should be(true)
  }

  it should "desugarize x[ys[0]][ys[1]] and make proper flattener tags" in {
    val (resVal, resTree) = valueToModel[InliningState](
      `raw x[ys[0]][ys[1]]`
    )
      .runA(InliningState(noNames = toMangler(Set("x", "ys"))))
      .value

    resVal should be(
      VarModel(
        "x",
        ArrayType(ArrayType(ScalarType.string)),
        Chain(
          IntoIndexModel("ys_flat", ArrayType(ScalarType.string)),
          IntoIndexModel("ys_flat-0", ScalarType.string)
        )
      )
    )

    resTree.isEmpty should be(false)

    resTree.get.equalsOrShowDiff(
      ParModel.wrap(
        FlattenModel(
          VarModel(
            "ys",
            ArrayType(ScalarType.i8),
            Chain.one(IntoIndexModel("0", ScalarType.i8))
          ),
          "ys_flat"
        ).leaf,
        FlattenModel(
          VarModel(
            "ys",
            ArrayType(ScalarType.i8),
            Chain.one(IntoIndexModel("1", ScalarType.i8))
          ),
          "ys_flat-0"
        ).leaf
      )
    ) should be(true)
  }

  it should "desugarize stream with gate" in {
    val stream = VarRaw("x", StreamType(ScalarType.string))
    val streamModel = VarModel.fromVarRaw(stream)
    val idxRaw = ysVarRaw(1)
    val streamWithProps = stream.withProperty(
      IntoIndexRaw(idxRaw, ScalarType.string)
    )

    val initState = InliningState(noNames = toMangler(Set("x", "ys")))

    // Here retrieve how size is inlined
    val (afterSizeState, (sizeModel, sizeTree)) =
      valueToModel[InliningState](idxRaw.increment).run(initState).value

    val (resVal, resTree) =
      valueToModel[InliningState](streamWithProps).runA(initState).value

    val idxModel = VarModel("x_idx", ScalarType.i8)

    val decrement = CallServiceModel(
      "math",
      "sub",
      List(
        sizeModel,
        LiteralModel.number(1)
      ),
      idxModel
    ).leaf

    val expected = SeqModel.wrap(
      sizeTree.toList :+
        join(streamModel, sizeModel) :+
        decrement
    )

    resVal should be(
      VarModel(
        "x_gate",
        ArrayType(ScalarType.string),
        Chain(
          IntoIndexModel(idxModel.name, ScalarType.string)
        )
      )
    )

    inside(resTree) { case Some(tree) =>
      tree.equalsOrShowDiff(expected) should be(true)
    }
  }

  it should "desugarize stream with length" in {
    val streamWithProps =
      VarRaw("x", StreamType(ScalarType.string)).withProperty(
        FunctorRaw("length", ScalarType.u32)
      )

    val (resVal, resTree) = valueToModel[InliningState](streamWithProps)
      .runA(InliningState(noNames = toMangler(Set("x", "ys"))))
      .value
  }

  it should "desugarize a recursive lambda value" in {
    val (resVal, resTree) = valueToModel[InliningState](
      `raw x[zs[ys[0]]][ys[1]]`
    )
      .runA(InliningState(noNames = toMangler(Set("x", "ys", "zs"))))
      .value

    // This is x[zs-0][ys-0]
    // zs-0 should be zs[ys[0]], which should be already flattened
    resVal should be(
      VarModel(
        "x",
        ArrayType(ArrayType(ScalarType.string)),
        Chain(
          IntoIndexModel("zs_flat", ArrayType(ScalarType.string)),
          IntoIndexModel("ys_flat-0", ScalarType.string)
        )
      )
    )

    resTree.isEmpty should be(false)

    resTree.get.equalsOrShowDiff(
      ParModel.wrap(
        FlattenModel(
          VarModel(
            "ys",
            ArrayType(ScalarType.i8),
            Chain.one(IntoIndexModel("0", ScalarType.i8))
          ),
          "ys_flat"
        ).leaf,
        FlattenModel(
          VarModel(
            "zs",
            ArrayType(ScalarType.i8),
            Chain.one(IntoIndexModel("ys_flat", ScalarType.i8))
          ),
          "zs_flat"
        ).leaf,
        // Now prepare ys-0
        FlattenModel(
          VarModel(
            "ys",
            ArrayType(ScalarType.i8),
            Chain.one(IntoIndexModel("1", ScalarType.i8))
          ),
          "ys_flat-0"
        ).leaf
      )
    ) should be(true)
  }

  it should "optimize constants comparison" in {

    for {
      l <- -100 to 100
      r <- -100 to 100
    } {
      val lt = valueToModel[InliningState](
        int(l) `<` int(r)
      ).runA(InliningState()).value

      lt shouldBe (
        LiteralModel.bool(l < r) -> None
      )

      val lte = valueToModel[InliningState](
        int(l) `<=` int(r)
      ).runA(InliningState()).value

      lte shouldBe (
        LiteralModel.bool(l <= r) -> None
      )

      val gt = valueToModel[InliningState](
        int(l) `>` int(r)
      ).runA(InliningState()).value

      gt shouldBe (
        LiteralModel.bool(l > r) -> None
      )

      val gte = valueToModel[InliningState](
        int(l) `>=` int(r)
      ).runA(InliningState()).value

      gte shouldBe (
        LiteralModel.bool(l >= r) -> None
      )
    }
  }

  it should "optimize constants math" in {
    for {
      l <- -100 to 100
      r <- -100 to 100
    } {
      val add = valueToModel[InliningState](
        int(l) `+` int(r)
      ).runA(InliningState()).value

      add shouldBe (
        LiteralModel.number(l + r) -> None
      )

      val sub = valueToModel[InliningState](
        int(l) `-` int(r)
      ).runA(InliningState()).value

      sub shouldBe (
        LiteralModel.number(l - r) -> None
      )

      val mul = valueToModel[InliningState](
        int(l) `*` int(r)
      ).runA(InliningState()).value

      mul shouldBe (
        LiteralModel.number(l * r) -> None
      )

      val div = valueToModel[InliningState](
        int(l) `/` int(r)
      ).runA(InliningState()).value

      val rem = valueToModel[InliningState](
        int(l) `%` int(r)
      ).runA(InliningState()).value

      if (r != 0)
        div shouldBe (
          LiteralModel.number(l / r) -> None
        )
        rem shouldBe (
          LiteralModel.number(l % r) -> None
        )
      else {
        val (dmodel, dtree) = div
        dmodel shouldBe a[VarModel]
        dtree.nonEmpty shouldBe (true)

        val (rmodel, rtree) = rem
        rmodel shouldBe a[VarModel]
        rtree.nonEmpty shouldBe (true)
      }

      if (r >= 0 && r <= 5) {
        val pow = valueToModel[InliningState](
          int(l) `**` int(r)
        ).runA(InliningState()).value

        pow shouldBe (
          LiteralModel.number(scala.math.pow(l, r).toLong) -> None
        )
      }
    }
  }

  it should "optimize addition in expressions" in {
    def test(numVars: Int, numLiterals: Int) = {
      val vars = (1 to numVars).map(i => ivar(s"v$i")).toList
      val literals = (1 to numLiterals).map(i => LiteralRaw.number(i)).toList
      val values = vars ++ literals

      /**
       * Enumerate all possible binary trees of vals
       */
      def genAllExprs(vals: List[ValueRaw]): List[ValueRaw] =
        if (vals.length <= 1) vals
        else
          for {
            split <- (1 until vals.length).toList
            (left, right) = vals.splitAt(split)
            l <- genAllExprs(left)
            r <- genAllExprs(right)
          } yield l `+` r

      for {
        perm <- values.permutations.toList
        expr <- genAllExprs(perm)
      } {
        val state = InliningState(
          resolvedExports = vars.map(v => v.name -> VarModel.fromVarRaw(v)).toMap
        )
        val (model, inline) = valueToModel[InliningState](expr).runA(state).value

        model shouldBe a[VarModel]
        inside(inline) { case Some(tree) =>
          val numberOfAdditions = Cofree
            .cata(tree) { (model, count: Chain[Int]) =>
              Eval.later {
                count.combineAll + (model match {
                  case CallServiceModel(_, "add", _) => 1
                  case _ => 0
                })
              }
            }
            .value

          numberOfAdditions shouldEqual numVars
        }
      }
    }

    /**
     * Number of expressions grows exponentially
     * So we test only small cases
     */
    test(2, 2)
    test(3, 2)
    test(2, 3)
  }
}
