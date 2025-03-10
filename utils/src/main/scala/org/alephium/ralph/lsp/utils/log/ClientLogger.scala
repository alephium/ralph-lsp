// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.utils.log

import com.typesafe.scalalogging.Logger

/** Defines APIs for publishing logs to an LSP client */
trait ClientLogger {

  def info(message: String)(implicit logger: Logger): Unit

  def warning(message: String)(implicit logger: Logger): Unit

  def error(message: String)(implicit logger: Logger): Unit

  def error(message: LogMessage)(implicit logger: Logger): Unit =
    this.error(message.message)

  def error(
      message: String,
      cause: Throwable
    )(implicit logger: Logger): Unit

  def debug(message: String)(implicit logger: Logger): Unit

  def trace(message: String)(implicit logger: Logger): Unit

}
