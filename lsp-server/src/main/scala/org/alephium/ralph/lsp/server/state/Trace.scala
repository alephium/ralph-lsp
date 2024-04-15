// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.server.state

import org.alephium.ralph.lsp.server.ResponseError.InvalidTraceSetting

/**
 * @see [[org.eclipse.lsp4j.TraceValue]].
 */
sealed trait Trace

object Trace {

  case object Off      extends Trace
  case object Messages extends Trace
  case object Verbose  extends Trace

  def apply(string: String): Either[InvalidTraceSetting, Trace] =
    if (string == null)
      Right(Trace.Off)
    else if (string equalsIgnoreCase Trace.Off.productPrefix)
      Right(Trace.Off)
    else if (string equalsIgnoreCase Trace.Messages.productPrefix)
      Right(Trace.Messages)
    else if (string equalsIgnoreCase Trace.Verbose.productPrefix)
      Right(Trace.Verbose)
    else
      Left(InvalidTraceSetting(string))

}
