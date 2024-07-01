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

package aqua.raw.ops

import aqua.raw.Raw
import aqua.raw.ops
import aqua.raw.value.{ValueRaw, VarRaw}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import cats.instances.tuple.*
import cats.kernel.Semigroup
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.Monad
import cats.data.State
import cats.data.StateT

case class FuncOp(tree: RawTag.Tree) extends Raw
