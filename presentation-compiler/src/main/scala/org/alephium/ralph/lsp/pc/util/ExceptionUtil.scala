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

package org.alephium.ralph.lsp.pc.util

import java.io.{StringWriter, PrintWriter}

object ExceptionUtil {

  /** Emit stack-trace as String */
  def toStringStackTrace(throwable: Throwable): String = {
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    throwable.printStackTrace(pw)
    sw.toString
  }

  /** Merge a message and exception into a String */
  def mergeToString(
      message: String,
      cause: Throwable): String = {
    val stringStackTrace = toStringStackTrace(cause)

    s"""$message
       |Cause: $stringStackTrace""".stripMargin
  }

}
