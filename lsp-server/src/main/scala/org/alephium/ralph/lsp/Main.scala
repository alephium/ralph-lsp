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

package org.alephium.ralph.lsp

import com.typesafe.scalalogging.StrictLogging
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.server.RalphLangServer
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.services.LanguageClient

import java.io.{InputStream, OutputStream}

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

    val server = RalphLangServer()

    // configure LSP server
    val launcher =
      new Launcher.Builder[LanguageClient]()
        .setInput(in)
        .setOutput(out)
        .setRemoteInterface(classOf[LanguageClient])
        .setLocalService(server)
        .create()

    server.setInitialState(
      client = launcher.getRemoteProxy,
      listener = launcher.startListening
    )
  }

}
