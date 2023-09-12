package org.alephium.ralph.lsp

import org.alephium.ralph.lsp.compiler.CompilerAccess
import org.alephium.ralph.lsp.server.{RalphLangClient, RalphLangServer}
import org.eclipse.lsp4j.jsonrpc.Launcher

import java.io.{InputStream, OutputStream}

object Main {

  def main(args: Array[String]): Unit = {
    scribe.info("Starting language server")

    implicit val compiler: CompilerAccess =
      CompilerAccess.ralphc

    // start lsp using standard IO
    start(System.in, System.out)
  }

  def start(in: InputStream, out: OutputStream)(implicit compiler: CompilerAccess): Unit = {
    val server = new RalphLangServer()

    // configure LSP server
    val launcher =
      new Launcher.Builder[RalphLangClient]()
        .setInput(in)
        .setOutput(out)
        .setRemoteInterface(classOf[RalphLangClient])
        .setLocalService(server)
        .create()

    server.setClient(launcher.getRemoteProxy)
    launcher.startListening().get()
  }
}
