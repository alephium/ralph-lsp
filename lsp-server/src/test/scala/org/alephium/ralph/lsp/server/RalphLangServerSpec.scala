package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.eclipse.lsp4j.{InitializeParams, InitializeResult}
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.net.URI
import java.util.concurrent.CompletableFuture

class RalphLangServerSpec extends AnyWordSpec with Matchers with MockFactory {

  "initialize" should {
    "set server workspaces and respond with capabilities" in {
      implicit val compiler: CompilerAccess =
        CompilerAccess.ralphc

      val client = mock[RalphLangClient]
      val listener = CompletableFuture.runAsync(() => ())
      val server = RalphLangServer(client, listener)

      // this is the initial message received from LSP client.
      val initialise = new InitializeParams()
      val workspaceURI = new URI("file://test")
      initialise.setRootUri(workspaceURI.toString)

      // invoke server with the initialise message
      val initializeResult = server.initialize(initialise).get()

      // expect server capabilities returned in response
      initializeResult shouldBe new InitializeResult(RalphLangServer.serverCapabilities())

      // the server must store the client and set the workspace in initialised state.
      server.getState() shouldBe
        ServerState(
          client = Some(client),
          listener = Some(listener),
          workspace = Some(WorkspaceState.Created(workspaceURI)),
          buildErrors = None,
        )
    }
  }

}
