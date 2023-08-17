package org.alephium.ralph.lsp

import org.alephium.ralph.lsp.server.RalphLangServer
import org.eclipse.lsp4j.{MessageParams, MessageType}
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.services.LanguageClient

import java.io.{InputStream, OutputStream}
import scala.concurrent.ExecutionContext

object Main {

  def main(args: Array[String]): Unit = {
    implicit val ec: ExecutionContext =
      ExecutionContext.Implicits.global

    // start lsp using standard IO
    start(System.in, System.out)
  }

  def start(in: InputStream, out: OutputStream)(implicit ec: ExecutionContext): Unit = {
    val server = new RalphLangServer()

    // configure LSP server
    val launcher =
      new Launcher.Builder[LanguageClient]()
        .setInput(in)
        .setOutput(out)
        .setRemoteInterface(classOf[LanguageClient])
        .setLocalService(server)
        .create()

    server.connect(launcher.getRemoteProxy)
    launcher.getRemoteProxy.logMessage(new MessageParams(MessageType.Info, "Server started")) // Temporary test log message
    launcher.startListening().get()
  }
}
