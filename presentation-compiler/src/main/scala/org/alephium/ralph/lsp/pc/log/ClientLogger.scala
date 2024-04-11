package org.alephium.ralph.lsp.pc.log

import com.typesafe.scalalogging.Logger

/** Defines APIs for publishing logs to an LSP client */
trait ClientLogger {

  def info(message: String)(implicit logger: Logger): Unit

  def warning(message: String)(implicit logger: Logger): Unit

  def error(message: String)(implicit logger: Logger): Unit

  def error(
      message: String,
      cause: Throwable
    )(implicit logger: Logger): Unit

  def debug(message: String)(implicit logger: Logger): Unit

  def trace(message: String)(implicit logger: Logger): Unit

}
