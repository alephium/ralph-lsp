package org.alephium.ralph.lsp

import com.typesafe.scalalogging.StrictLogging
import org.alephium.ralph.lsp.compiler.CompilerAccess
import org.alephium.ralph.lsp.server.{RalphLangClient, RalphLangServer}
import org.eclipse.lsp4j.jsonrpc.Launcher

import java.io.{InputStream, OutputStream}

object Main extends StrictLogging {

  def main(args: Array[String]): Unit = {
    logger.info("Starting Ralph-LSP")
    start(System.in, System.out) // start lsp using standard IO
    logger.info("Ralph-LSP started!")
  }

  def start(in: InputStream, out: OutputStream): Unit = {
    implicit val compiler: CompilerAccess =
      CompilerAccess.ralphc

    val server = new RalphLangServer()

    // configure LSP server
    val launcher =
      new Launcher.Builder[RalphLangClient]()
        .setInput(in)
        .setOutput(out)
        .setRemoteInterface(classOf[RalphLangClient])
        .setLocalService(server)
        .create()

    server.setInitialState(
      client = launcher.getRemoteProxy,
      listener = launcher.startListening
    )
  }
}
