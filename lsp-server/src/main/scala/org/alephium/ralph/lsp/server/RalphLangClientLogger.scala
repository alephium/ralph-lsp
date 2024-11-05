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

package org.alephium.ralph.lsp.server

import com.typesafe.scalalogging.Logger
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.alephium.ralph.lsp.server.state.Trace

/**
 * Logs messages to local slf4j configuration and to remote LSP client.
 *
 * @param client The remote client proxy.
 * @param trace  Current trace setting configured by the LSP client.
 */
case class RalphLangClientLogger(
    client: RalphLangClient,
    trace: Trace)
  extends ClientLogger {

  override def info(message: String)(implicit logger: Logger): Unit = {
    logger.info(message)
    client.info(message)
  }

  override def warning(message: String)(implicit logger: Logger): Unit = {
    logger.warn(message)
    client.warning(message)
  }

  override def error(message: String)(implicit logger: Logger): Unit = {
    logger.error(message)
    client.error(message)
  }

  override def error(
      message: String,
      cause: Throwable
    )(implicit logger: Logger): Unit = {
    logger.error(message, cause)
    client.error(message, cause)
  }

  override def debug(message: String)(implicit logger: Logger): Unit = {
    logger.debug(message)
    if (trace != Trace.Off)
      client.log(s"[Debug] $message")
  }

  override def trace(message: String)(implicit logger: Logger): Unit = {
    logger.trace(message)
    if (trace == Trace.Verbose)
      client.trace(message)
  }

}
