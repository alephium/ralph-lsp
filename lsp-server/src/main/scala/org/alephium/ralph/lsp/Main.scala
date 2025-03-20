// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp

import com.typesafe.scalalogging.StrictLogging
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.server.RalphLangServer
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.services.LanguageClient

import java.io.{InputStream, OutputStream}
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object Main extends StrictLogging {

  def main(args: Array[String]): Unit = {
    logger.info("Starting Ralph-LSP")
    start(System.in, System.out) // start lsp using standard IO
    logger.info("Ralph-LSP started!")
  }

  def start(
      in: InputStream,
      out: OutputStream): Unit = {
    implicit val compiler: CompilerAccess =
      CompilerAccess.ralphc

    implicit val file: FileAccess =
      FileAccess.disk

    val executor =
      Executors.newCachedThreadPool()

    implicit val ec: ExecutionContext =
      ExecutionContext.fromExecutor(executor)

    val server = RalphLangServer()

    // configure LSP server
    val launcher =
      new Launcher.Builder[LanguageClient]()
        .setInput(in)
        .setOutput(out)
        .setRemoteInterface(classOf[LanguageClient])
        .setLocalService(server)
        .setExecutorService(executor)
        .create()

    server.setInitialState(
      client = launcher.getRemoteProxy,
      listener = launcher.startListening
    )
  }

}
