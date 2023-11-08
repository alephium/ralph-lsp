package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.server.state.ServerState
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.jdk.FutureConverters._
import java.net.URI
import java.util.concurrent.{ CompletionStage,CompletableFuture, Future => JFuture}
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.Promise

class RalphLangServerSpec extends AnyWordSpec with Matchers with MockFactory with ScalaFutures{

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
          workspace = Some(WorkspaceState.Created(workspaceURI)),
          buildErrors = None,
          clientCapabilities = Some(initialise.getCapabilities),
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
    implicit val compiler: CompilerAccess = CompilerAccess.ralphc
    implicit val file: FileAccess = FileAccess.disk
    val client : RalphLangClient = new EmptyRalphLangClient{}
    val listener = CompletableFuture.runAsync(() => ())

    "return true, set `shutdownReceived` to true, but not cancel the listener" in {
      val server = RalphLangServer(client, listener)

      server.getState().shutdownReceived shouldBe false
      server.shutdown().asScala.futureValue shouldBe true
      server.getState().shutdownReceived shouldBe true
      server.getState().listener.get.isCancelled shouldBe false
    }

    "return an `InvalidRequest` when receiving more than one shutdown request" in {
      val server = RalphLangServer(client, listener)

      server.shutdown().asScala.futureValue shouldBe true
      //Testing messages as the two java classes aren't consider equal
      server.shutdown().asScala.failed.futureValue.getMessage shouldBe ResponseError.ShutdownRequested.toResponseErrorException.getMessage
    }
  }

  "exit" should {
    implicit val compiler: CompilerAccess = CompilerAccess.ralphc
    implicit val file: FileAccess = FileAccess.disk
    val client : RalphLangClient = new EmptyRalphLangClient{}

    //We use a Promise to have a non-completed future
    val listener = Promise[Void]().future.asJava.asInstanceOf[JFuture[Void]]

    "cancel listener and exit successfully if shutdown was triggered" in {
      val server = RalphLangServer(client, listener)

      server.getState().listener.get.isCancelled shouldBe false
      server.shutdown().asScala.futureValue shouldBe true

      server.exitWithCode() shouldBe 0
      server.getState().listener.get.isCancelled shouldBe true
   }

    "exit with error if shutdown wasn't requested" in {
      val server = RalphLangServer(client, listener)

      server.exitWithCode() shouldBe 1
      server.getState().listener.get.isCancelled shouldBe true
   }
  }

  "initialized" should {

    def buildServer(dynamicRegistration: Option[Boolean], registered: AtomicBoolean) = {
      implicit val compiler: CompilerAccess = CompilerAccess.ralphc
      implicit val file: FileAccess = FileAccess.disk
      val client: RalphLangClient = new EmptyRalphLangClient {
        override def registerCapability(params: RegistrationParams): CompletableFuture[Void] = {
          registered.set(true)
          new CompletableFuture[Void]()
        }
      }
      val listener = CompletableFuture.runAsync(() => ())
      val server = RalphLangServer(client, listener)

      val capabilities = dynamicRegistration match {
        case None => new ClientCapabilities()
        case Some(dynamicRegistration) =>
          val watchedFilesCapabilities = new DidChangeWatchedFilesCapabilities(dynamicRegistration)
          val workspaceCapabilities = new WorkspaceClientCapabilities()
          workspaceCapabilities.setDidChangeWatchedFiles(watchedFilesCapabilities)
          val result = new ClientCapabilities()
          result.setWorkspace(workspaceCapabilities)
          result
      }

      val initialise = new InitializeParams()
      val workspaceURI = new URI("file://test")
      initialise.setRootUri(workspaceURI.toString)
      initialise.setCapabilities(capabilities)

      server.initialize(initialise).get()
      server
    }

    "register watched files if dynamic registration is true" in {
      val registered = new AtomicBoolean(false)
      val server = buildServer(Some(true), registered)
      server.initialized(new InitializedParams())

      registered.get shouldBe true
    }

    "not register watched files if dynamic registration is false" in {
      val registered = new AtomicBoolean(false)
      val server = buildServer(Some(false), registered)
      server.initialized(new InitializedParams())

      registered.get shouldBe false
    }

    "not register watched files if dynamic registration is not set" in {
      val registered = new AtomicBoolean(false)
      val server = buildServer(None, registered)
      server.initialized(new InitializedParams())

      registered.get shouldBe false
    }
  }
}

trait EmptyRalphLangClient extends RalphLangClient {
  def logMessage(x: org.eclipse.lsp4j.MessageParams): Unit = ()
  def publishDiagnostics(x: org.eclipse.lsp4j.PublishDiagnosticsParams): Unit = ()
  def showMessage(x: org.eclipse.lsp4j.MessageParams): Unit = ()
  def showMessageRequest(x: org.eclipse.lsp4j.ShowMessageRequestParams): java.util.concurrent.CompletableFuture[org.eclipse.lsp4j.MessageActionItem] = ???
  def telemetryEvent(x: Object): Unit = ()
}
