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

package aqua

import cats.data.{Chain, EitherT, NonEmptyChain, Writer}

package object api {

  type APIWarnings = [A] =>> Writer[
    Chain[String],
    A
  ]

  type APIResult = [A] =>> EitherT[
    APIWarnings,
    NonEmptyChain[String],
    A
  ]
}
