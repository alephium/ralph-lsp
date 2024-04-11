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
