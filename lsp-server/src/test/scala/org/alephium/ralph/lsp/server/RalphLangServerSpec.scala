package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.state.PCState
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.server.state.{ServerState, Trace}
import org.eclipse.lsp4j._
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.net.URI
import java.util.concurrent.{CompletableFuture, Future => JFuture}
import scala.concurrent.Promise
import scala.jdk.FutureConverters._

class RalphLangServerSpec extends AnyWordSpec with Matchers with MockFactory with ScalaFutures {

  "initialize" should {
    "set server workspaces and respond with capabilities" in {
      implicit val compiler: CompilerAccess =
        CompilerAccess.ralphc

      implicit val file: FileAccess =
        FileAccess.disk

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
          pcState =
            Some(
              PCState(
                workspace = WorkspaceState.Created(workspaceURI),
                buildErrors = None
              )
            ),
          clientAllowsWatchedFilesDynamicRegistration = false,
          trace = Trace.Off,
          shutdownReceived = false
        )
    }
  }

  "getRootUri" should {
    "Create valide URI with scheme when no RootUri or RootPath exists" in {
      val initialise = new InitializeParams()
      val uri = RalphLangServer.getRootUri(initialise)

      uri.get.getScheme() shouldBe "file"
    }
  }

  "shutdown" should {
    implicit val compiler: CompilerAccess = null
    implicit val file: FileAccess = null
    val listener = CompletableFuture.runAsync(() => ())

    "return true, set `shutdownReceived` to true, but not cancel the listener" in {
      val client = mock[RalphLangClient]

      val server = RalphLangServer(client, listener)

      // ignore calls to info
      (client.info _)
        .expects(*)
        .anyNumberOfTimes()

      server.getState().shutdownReceived shouldBe false
      server.shutdown().asScala.futureValue shouldBe true
      server.getState().shutdownReceived shouldBe true
      server.getState().listener.get.isCancelled shouldBe false
    }

    "return an `InvalidRequest` when receiving more than one shutdown request" in {
      val client = mock[RalphLangClient]

      val server = RalphLangServer(client, listener)

      // ignore calls to info
      (client.info _)
        .expects(*)
        .anyNumberOfTimes()

      server.shutdown().asScala.futureValue shouldBe true

      // shutdown error message is logged to the client once
      (client.error(_: String, _: Throwable))
        .expects(ResponseError.ShutdownRequested.getMessage, *)
        .once()

      //Testing messages as the two java classes aren't consider equal
      server.shutdown().asScala.failed.futureValue.getMessage shouldBe ResponseError.ShutdownRequested.toResponseErrorException.getMessage
    }
  }

  "exit" should {
    implicit val compiler: CompilerAccess = CompilerAccess.ralphc
    implicit val file: FileAccess = FileAccess.disk
    //We use a Promise to have a non-completed future
    val listener = Promise[Void]().future.asJava.asInstanceOf[JFuture[Void]]

    "cancel listener and exit successfully if shutdown was triggered" in {
      val client = mock[RalphLangClient]

      // ignore calls to info
      (client.info _)
        .expects(*)
        .anyNumberOfTimes()

      val server = RalphLangServer(client, listener)

      server.getState().listener.get.isCancelled shouldBe false
      server.shutdown().asScala.futureValue shouldBe true

      server.exitWithCode() shouldBe 0
      server.getState().listener.get.isCancelled shouldBe true
    }

    "exit with error if shutdown wasn't requested" in {
      val client = mock[RalphLangClient]

      // ignore calls to info
      (client.info _)
        .expects(*)
        .anyNumberOfTimes()

      val server = RalphLangServer(client, listener)

      server.exitWithCode() shouldBe 1
      server.getState().listener.get.isCancelled shouldBe true
    }
  }

  "registerClientCapabilities" should {
    "register watched files if dynamic registration is true" in {
      implicit val compiler: CompilerAccess = null // compile is never accessed
      implicit val file: FileAccess = null // file/disk IO is never accessed

      val client = mock[RalphLangClient]

      val server =
        RalphLangServer(
          client = client,
          listener = CompletableFuture.runAsync(() => ()),
          clientAllowsWatchedFilesDynamicRegistration = true
        )

      server.getState().clientAllowsWatchedFilesDynamicRegistration shouldBe true

      // register is called once with the default client-capabilities
      (client.register _)
        .expects(RalphLangServer.clientCapabilities())
        .returning(new CompletableFuture[Void]())
        .once()

      server.registerClientCapabilities() shouldBe (())
    }

    "not register watched files if dynamic registration is false" in {
      implicit val compiler: CompilerAccess = null // compile is never accessed
      implicit val file: FileAccess = null // file/disk IO is never accessed

      val client = mock[RalphLangClient]

      val server =
        RalphLangServer(
          client = client,
          listener = CompletableFuture.runAsync(() => ()),
          clientAllowsWatchedFilesDynamicRegistration = false
        )

      server.getState().clientAllowsWatchedFilesDynamicRegistration shouldBe false

      // register is never called
      (client.register _)
        .expects(*)
        .never()

      server.registerClientCapabilities() shouldBe (())
    }
  }
}
