// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.utils

import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}

import scala.jdk.OptionConverters.RichOptional

object ProcessUtil extends StrictImplicitLogging {

  /**
   * Shuts down the server if the process either exits or cannot be found.
   *
   * @param processId The ID of the process to monitor. If null, the server will
   *                  immediately initiate a shutdown.
   */
  def closeOnExit(
      processId: Integer,
      closeable: AutoCloseable
    )(implicit logger: ClientLogger): Unit =
    Option(processId) match {
      case Some(pid) =>
        logger.trace(s"Monitoring exit of process with ID $pid.")
        if (pid == ProcessHandle.current().pid().toInt)
          logger.debug(s"Monitoring exit of current process $pid is not allowed.")
        else
          ProcessHandle.of(pid.toLong).toScala match {
            case Some(processHandle) =>
              val _ =
                processHandle
                  .onExit()
                  .thenRun {
                    () =>
                      logger.trace(s"Parent process with ID $pid has terminated. Initiating server shutdown.")
                      closeable.close()
                  }
                  .exceptionally {
                    throwable =>
                      logger.error(s"Error during ProcessHandle exit for PID $pid. Initiating forced server shutdown.", throwable)
                      closeable.close()
                      null
                  }

            case None =>
              logger.error(s"Process with ID $pid not found. Initiating server shutdown.")
              closeable.close()
          }

      case None =>
        logger.error("Provided process ID is null. Initiating server shutdown.")
        closeable.close()
    }

}
