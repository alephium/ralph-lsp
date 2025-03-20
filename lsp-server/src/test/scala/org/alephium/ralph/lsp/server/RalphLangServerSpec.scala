// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.{PCStates, PCState}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.server.state.{ServerState, Trace}
import org.eclipse.lsp4j._
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

import java.nio.file.Paths
import java.util
import java.util.concurrent.{CompletableFuture, Future => JFuture}
import scala.collection.immutable.ArraySeq
import scala.concurrent.ExecutionContext.Implicits.global
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
      // format: off
      val listener = CompletableFuture.runAsync(() => ()) // format: on
      val server   = RalphLangServer(client, listener)

      // this is the initial message received from the LSP client.
      val initialise    = new InitializeParams()
      val workspaceURI1 = Paths.get("workspace1").toUri
      val workspaceURI2 = Paths.get("workspace2").toUri
      val workspaceURI3 = Paths.get("workspace1").toUri // This has the same directory as workspace1 and should be ignored
      val workspaceFolders =
        util
          .Arrays
          .asList(
            new WorkspaceFolder(workspaceURI1.toString, ""),
            new WorkspaceFolder(workspaceURI2.toString, ""),
            new WorkspaceFolder(workspaceURI3.toString, "")
          )

      initialise.setWorkspaceFolders(workspaceFolders)
      initialise.setProcessId(ProcessHandle.current().pid().toInt)

      // invoke server with the 'initialise' message
      val initializeResult = server.initialize(initialise).get()

      // expect server capabilities returned in response
      initializeResult shouldBe new InitializeResult(RalphLangServer.serverCapabilities())

      // the server must store the client and set the workspace in initialised state.
      server.getState() shouldBe
        ServerState(
          client = Some(client),
          listener = Some(listener),
          pcStates =
            // Note the duplicate workspaceURI3 is not created
            PCStates(
              ArraySeq(
                PCState(
                  workspace = WorkspaceState.Created(workspaceURI1),
                  buildErrors = None,
                  tsErrors = None
                ),
                PCState(
                  workspace = WorkspaceState.Created(workspaceURI2),
                  buildErrors = None,
                  tsErrors = None
                )
              )
            ),
          clientAllowsWatchedFilesDynamicRegistration = false,
          trace = Trace.Off,
          shutdownReceived = false
        )
    }
  }

  "getWorkspaceFolders" should {
    "Create valid URI with scheme when no RootUri or RootPath exists" in {
      val initialise = new InitializeParams()
      val uri        = RalphLangServer.getWorkspaceFolders(initialise).value

      uri should have size 1
      uri.head.getScheme() shouldBe "file"
    }
  }

  "shutdown" should {
    implicit val compiler: CompilerAccess = null
    implicit val file: FileAccess         = null
    // format: off
    val listener = CompletableFuture.runAsync(() => ())
    // format: on

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
      (client
        .error(_: String, _: Throwable))
        .expects(ResponseError.ShutdownRequested.getMessage, *)
        .once()

      // Testing messages as the two java classes aren't consider equal
      server.shutdown().asScala.failed.futureValue.getMessage shouldBe ResponseError.ShutdownRequested.toResponseErrorException.getMessage
    }
  }

  "exit" should {
    implicit val compiler: CompilerAccess = CompilerAccess.ralphc
    implicit val file: FileAccess         = FileAccess.disk
    // We use a Promise to have a non-completed future
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
      implicit val file: FileAccess         = null // file/disk IO is never accessed

      val client = mock[RalphLangClient]

      val server =
        RalphLangServer(
          client = client,
          // format: off
          listener = CompletableFuture.runAsync(() => ()), // format: on
          clientAllowsWatchedFilesDynamicRegistration = true
        )

      server.getState().clientAllowsWatchedFilesDynamicRegistration shouldBe true

      // register is called once with the default client-capabilities
      (client.register _)
        .expects(RalphLangServer.clientCapabilities())
        .returning(new CompletableFuture[Void]())
        .once()

      server.registerClientCapabilities() shouldBe ()
    }

    "not register watched files if dynamic registration is false" in {
      implicit val compiler: CompilerAccess = null // compile is never accessed
      implicit val file: FileAccess         = null // file/disk IO is never accessed

      val client = mock[RalphLangClient]

      val server =
        RalphLangServer(
          client = client,
          // format: off
          listener = CompletableFuture.runAsync(() => ()), // format: on
          clientAllowsWatchedFilesDynamicRegistration = false
        )

      server.getState().clientAllowsWatchedFilesDynamicRegistration shouldBe false

      // register is never called
      (client.register _)
        .expects(*)
        .never()

      server.registerClientCapabilities() shouldBe ()
    }
  }

}
