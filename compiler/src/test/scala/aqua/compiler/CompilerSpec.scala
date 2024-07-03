/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.compiler

import aqua.compiler.FileIdString.given
import aqua.model.AquaContext
import aqua.model.transform.{Transform, TransformConfig}
import aqua.parser.lift.Span
import aqua.parser.{Parser, ParserError}
import aqua.raw.ConstantRaw
import aqua.res.FuncRes

import cats.Id
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.syntax.either.*
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

trait CompilerSpec extends AnyFlatSpec with Matchers with Inside {

  private def aquaSource(src: Map[String, String], imports: Map[String, String]) = {
    new AquaSources[Id, String, String] {

      override def sources: Id[ValidatedNec[String, Chain[(String, String)]]] =
        Validated.validNec(Chain.fromSeq(src.toSeq))

      override def resolveImport(from: String, imp: String): Id[ValidatedNec[String, String]] =
        Validated.validNec(imp)

      override def load(file: String): Id[ValidatedNec[String, String]] =
        Validated.fromEither(
          (imports ++ src)
            .get(file)
            .toRight(NonEmptyChain.one(s"Cannot load imported file $file"))
        )
    }
  }

  protected def insideContext(
    src: Map[String, String],
    imports: Map[String, String] = Map.empty
  )(test: AquaContext => Any) = {
    val compiled = CompilerAPI
      .compileToContext[Id, String, String, Span.S](
        aquaSource(src, imports),
        id => txt => Parser.parse(Parser.parserSchema)(txt),
        AquaCompilerConf(ConstantRaw.defaultConstants(None))
      )
      .value
      .value
      .toValidated

    inside(compiled) { case Validated.Valid(contexts) =>
      inside(contexts.headOption) { case Some(ctx) =>
        test(ctx)
      }
    }
  }

  protected def insideRes(
    src: Map[String, String],
    imports: Map[String, String] = Map.empty,
    transformCfg: TransformConfig = TransformConfig()
  )(funcNames: String*)(
    test: PartialFunction[List[FuncRes], Any]
  ) = insideContext(src, imports)(ctx =>
    val aquaRes = Transform.contextRes(ctx, transformCfg)
    // To preserve order as in funcNames do flatMap
    val funcs = funcNames.flatMap(name => aquaRes.funcs.find(_.funcName == name)).toList
    inside(funcs)(test)
  )

}
