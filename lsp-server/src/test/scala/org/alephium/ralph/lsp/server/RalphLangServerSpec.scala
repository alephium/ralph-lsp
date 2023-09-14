package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.eclipse.lsp4j.{InitializeParams, InitializeResult, WorkspaceFolder}
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.net.URI
import scala.jdk.CollectionConverters.SeqHasAsJava

class RalphLangServerSpec extends AnyWordSpec with Matchers with MockFactory {

  "initialize" should {
    ""

    "set server workspaces and respond with capabilities" in {
      implicit val compiler: CompilerAccess =
        CompilerAccess.ralphc

      val client = mock[RalphLangClient]
      val server = RalphLangServer(client)

      // this is the initial message received from LSP client.
      val initialise = new InitializeParams()
      val workspaceURI = new URI("file://test")
      val workspaceFolders = List(new WorkspaceFolder(workspaceURI.toString))
      initialise.setWorkspaceFolders(workspaceFolders.asJava)

      // invoke server with the initialise message
      val initializeResult = server.initialize(initialise).get()

      // expect server capabilities returned in response
      initializeResult shouldBe new InitializeResult(RalphLangServer.serverCapabilities())

      // the server must store the client and set the workspace in un-configured state.
      server.getState() shouldBe
        ServerState(
          client = Some(client),
          workspace = Some(WorkspaceState.UnConfigured(workspaceURI))
        )
    }
  }

}
