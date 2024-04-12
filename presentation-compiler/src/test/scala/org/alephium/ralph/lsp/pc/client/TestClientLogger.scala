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

package org.alephium.ralph.lsp.pc.client

import com.typesafe.scalalogging.Logger
import org.alephium.ralph.lsp.pc.log.ClientLogger

/** Test logger that has no remote client. This simply logs to SLF4J. */
object TestClientLogger extends ClientLogger {

  override def info(message: String)(implicit logger: Logger): Unit =
    logger.info(message)

  override def warning(message: String)(implicit logger: Logger): Unit =
    logger.warn(message)

  override def error(message: String)(implicit logger: Logger): Unit =
    logger.error(message)

  override def error(
      message: String,
      cause: Throwable
    )(implicit logger: Logger): Unit =
    logger.error(message, cause)

  override def debug(message: String)(implicit logger: Logger): Unit =
    logger.debug(message)

  override def trace(message: String)(implicit logger: Logger): Unit =
    logger.trace(message)

}
