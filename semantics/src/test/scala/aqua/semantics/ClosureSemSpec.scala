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

package aqua.semantics

import aqua.parser.expr.func.ClosureExpr
import aqua.parser.lexer.{Name, Token}
import aqua.raw.arrow.ArrowRaw
import aqua.raw.Raw
import aqua.raw.ops.{EmptyTag, FuncOp, RawTag}
import aqua.semantics.expr.func.ClosureSem
import aqua.semantics.rules.names.{NamesInterpreter, NamesState}
import aqua.types.{ArrowType, ProductType}
import cats.data.*
import cats.data.State.*
import cats.instances.all.*
import cats.syntax.all.*
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.bifunctor.*
import cats.syntax.comonad.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monad.*
import cats.syntax.semigroup.*
import cats.{catsInstancesForId, Id, Monad}
import monocle.Lens
import monocle.macros.GenLens
import monocle.syntax.all.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ClosureSemSpec extends AnyFlatSpec with Matchers {

  import Utils.{given, *}

  val program: Prog[State[CompilerState[cats.Id], *], Raw] = {
    import CompilerState.*
    val expr = ClosureExpr(Name[Id]("closure"), None)
    val sem = new ClosureSem[Id](expr)

    sem.program[State[CompilerState[Id], *]]
  }

  "sem" should "create right model" in {

    val at =
      ArrowRaw(ArrowType(ProductType(Nil), ProductType(Nil)), Nil, RawTag.empty)
    val model = getModel(program.wrap(blank, (_, _) => State.pure(at)))
    model shouldBe (at)
  }

  "sem" should "create an error" in {
    val model = getModel(program)
    model shouldBe (Raw.Empty("Closure must continue with an arrow definition"))
  }
}
