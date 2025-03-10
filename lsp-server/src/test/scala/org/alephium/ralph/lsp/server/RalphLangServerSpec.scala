// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.PCState
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.alephium.ralph.lsp.pc.sourcecode.TestSourceCode
import org.alephium.ralph.lsp.pc.workspace.{WorkspaceState, TestWorkspace}
import org.alephium.ralph.lsp.server.state.{Trace, ServerState}
import org.eclipse.lsp4j._
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

import java.nio.file.Paths
import java.util.concurrent.{CompletableFuture, Future => JFuture}
import scala.annotation.nowarn
import scala.concurrent.Promise
import scala.jdk.FutureConverters._

@nowarn("cat=deprecation")
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
      val initialise   = new InitializeParams()
      val workspaceURI = Paths.get("test").toUri
      initialise.setRootUri(workspaceURI.toString)
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
          pcState = Some(
            PCState(
              workspace = WorkspaceState.Created(workspaceURI),
              buildErrors = None,
              tsErrors = None
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
      val uri        = RalphLangServer.getRootUri(initialise)

      uri.get.getScheme() shouldBe "file"
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

  "reboot" should {
    "start a new workspace compilation" in {
      implicit val compiler: CompilerAccess = CompilerAccess.ralphc
      implicit val file: FileAccess         = FileAccess.disk
      implicit val logger: ClientLogger     = TestClientLogger

      val client = mock[RalphLangClient]
      // format: off
      val listener = CompletableFuture.runAsync(() => ()) // format: on
      val server   = RalphLangServer(client, listener)

      // create unCompiled workspace
      val workspace =
        TestWorkspace
          .genUnCompiled(
            """
              |Contract Test() {
              |  fn main() -> () {}
              |}
              |""".stripMargin
          )
          .sample
          .get

      //  persist the workspace
      TestWorkspace persist workspace

      /**
       * Start LSP server for that workspace
       */
      // this is the initial message received from the LSP client.
      val initialise = new InitializeParams()
      initialise.setRootUri(workspace.workspaceURI.toString)
      initialise.setProcessId(ProcessHandle.current().pid().toInt)
      // invoke server with the 'initialise' message
      val initializeResult = server.initialize(initialise).get()
      // expect server capabilities returned in response
      initializeResult shouldBe new InitializeResult(RalphLangServer.serverCapabilities())

      // Invoke initialized so an initial build occurs.
      // After initialized, expect diagnostics to get published twice invoked by triggerInitialBuild():
      //  1) First for the `alephium.config.ts` build,
      //  2) Then for the `ralph.json` build.
      (client.publish _).expects(*).twice()
      server.initialized(new InitializedParams()) shouldBe ()

      // The server has created a workspace in Compiled state
      val newWorkspace = server.getState().pcState.value.workspace.asInstanceOf[WorkspaceState.Compiled]
      newWorkspace.sourceCode.map(_.fileURI) should contain only workspace.sourceCode.head.fileURI

      // Write a new file to the workspace
      val newFile =
        TestSourceCode
          .genUnCompiled(
            code = """
              |Contract Test2() {
              |  fn main() -> () {}
              |}
              |""".stripMargin,
            fileURI = workspace.build.contractURI.resolve("newFile.ral")
          )
          .sample
          .get

      // persist the new file
      TestSourceCode persist newFile

      // Expect diagnostics to get published three times on reboot:
      // 1) Publish diagnostics to clear all existing errors & warning.
      // 2) Publish diagnostics for the new builds invoked by triggerInitialBuild():
      //    2.1) First for the `alephium.config.ts` build,
      //    2.2) Then for the `ralph.json` build.
      (client.publish _).expects(*).repeat(3)

      // invoke reboot, which should rebuild the workspace, but this time there will be 2 source files
      server.reboot()

      // Now the server has two source files instead of one.
      val rebootedWorkspace = server.getState().pcState.value.workspace.asInstanceOf[WorkspaceState.Compiled]
      rebootedWorkspace.sourceCode.map(_.fileURI) should contain only (workspace.sourceCode.head.fileURI, newFile.fileURI)
    }
  }

}
