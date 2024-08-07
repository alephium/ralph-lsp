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

package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.diagnostic.Diagnostics
import org.alephium.ralph.lsp.pc.log.StrictImplicitLogging
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.search.completion.Suggestion
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.util.CollectionUtil
import org.alephium.ralph.lsp.pc.util.URIUtil.{isFileScheme, uri}
import org.alephium.ralph.lsp.pc.workspace._
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorUnknownFileType
import org.alephium.ralph.lsp.pc.{PCState, PC}
import org.alephium.ralph.lsp.server
import org.alephium.ralph.lsp.server.MessageMethods.{WORKSPACE_WATCHED_FILES_ID, WORKSPACE_WATCHED_FILES}
import org.alephium.ralph.lsp.server.RalphLangServer._
import org.alephium.ralph.lsp.server.converter.{DiagnosticsConverter, GoToConverter, CompletionConverter}
import org.alephium.ralph.lsp.server.state.{Trace, ServerState}
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.{CancelChecker, messages, CompletableFutures}
import org.eclipse.lsp4j.services._

import java.net.URI
import java.nio.file.Paths
import java.util
import java.util.concurrent.{CompletableFuture, Future => JFuture}
import scala.annotation.nowarn
import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters.IterableHasAsScala

object RalphLangServer {

  /** Start server with pre-configured client */
  def apply(
      client: RalphLangClient,
      listener: JFuture[Void],
      clientAllowsWatchedFilesDynamicRegistration: Boolean = false
    )(implicit compiler: CompilerAccess,
      file: FileAccess): RalphLangServer = {
    val initialState =
      ServerState(
        client = Some(client),
        listener = Some(listener),
        pcState = None,
        clientAllowsWatchedFilesDynamicRegistration = clientAllowsWatchedFilesDynamicRegistration,
        trace = Trace.Off,
        shutdownReceived = false
      )

    new RalphLangServer(initialState)
  }

  // format: off
  def apply()(implicit compiler: CompilerAccess,
              file: FileAccess): RalphLangServer = // format: on
    new RalphLangServer(
      ServerState(
        client = None,
        listener = None,
        pcState = None,
        clientAllowsWatchedFilesDynamicRegistration = false,
        trace = Trace.Off,
        shutdownReceived = false
      )
    )

  @nowarn("cat=deprecation")
  def getRootUri(params: InitializeParams): Option[URI] =
    Option(params.getRootUri)
      .orElse(Option(params.getRootPath))
      .map(URI.create)
      // Some LSP clients aren't providing `rootUri` or `rootPath`, like in nvim, so we fall back on `user.dir`
      .orElse {
        Option(System.getProperty("user.dir"))
          .map(Paths.get(_))
          .map(_.toUri)
      }

  /** Build capabilities supported by the LSP server */
  def serverCapabilities(): ServerCapabilities = {
    val capabilities = new ServerCapabilities()

    capabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
    capabilities.setCompletionProvider(new CompletionOptions(false, util.Arrays.asList(".")))
    capabilities.setDefinitionProvider(true)

    capabilities
  }

  /** Build capabilities needed by from the server */
  def clientCapabilities(): Registration = {
    val watchers = java.util.Arrays.asList(new FileSystemWatcher(messages.Either.forLeft("**/*")))
    val options  = new DidChangeWatchedFilesRegistrationOptions(watchers)
    new Registration(WORKSPACE_WATCHED_FILES_ID, WORKSPACE_WATCHED_FILES, options)
  }

  /** Checks if the client all dynamic registeration of watched file */
  def getAllowsWatchedFilesDynamicRegistration(params: InitializeParams): Boolean = {
    val allows: Option[Boolean] =
      for {
        capabilities          <- Option(params.getCapabilities())
        workspace             <- Option(capabilities.getWorkspace())
        didChangeWatchedFiles <- Option(workspace.getDidChangeWatchedFiles())
        dynamicRegistration   <- Option(didChangeWatchedFiles.getDynamicRegistration())
      } yield dynamicRegistration

    allows contains true
  }

}

/**
 * The Ralph-LSP server.
 *
 * This class is the only one with mutable state in this repo.
 * All mutable state management occurs here.
 */
class RalphLangServer private (
    @volatile private var state: ServerState
  )(implicit compiler: CompilerAccess,
    file: FileAccess)
  extends LanguageServer
     with TextDocumentService
     with WorkspaceService
     with StrictImplicitLogging { thisServer =>

  def getState(): ServerState =
    thisServer.state

  private implicit def logger: RalphLangClientLogger =
    RalphLangClientLogger(
      client = getClient(),
      trace = state.trace
    )

  /**
   * An initial call to this function is required before this server can start processing request.
   *
   * @param client   Client proxy instance provided by LSP4J.
   *                 Client must be known before a connection is initialised.
   * @param listener LSP connection listener function.
   */
  def setInitialState(
      client: LanguageClient,
      listener: () => JFuture[Void]): Unit =
    runSync {
      require(state.client.isEmpty, "Client is already set")
      require(state.listener.isEmpty, "Listener is already set")

      // Client must be set first, before running the request listener,
      // so that it is available for responding to requests.
      val ralphClient = RalphLangClient(client)
      thisServer.state = state.copy(client = Some(ralphClient))
      thisServer.state = state.copy(listener = Some(listener()))
    }

  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] =
    runAsync {
      cancelChecker =>
        logger.debug("Initialize request")
        // Previous commit uses the non-deprecated API but that does not work in vim.
        val rootURI =
          RalphLangServer.getRootUri(params)

        val workspaceURI =
          rootURI getOrElse notifyAndThrow(ResponseError.WorkspaceFolderNotSupplied)

        val pcState =
          PC.initialise(workspaceURI)

        setPCState(pcState)

        val maybeDynamicRegistration =
          getAllowsWatchedFilesDynamicRegistration(params)

        setClientAllowsWatchedFilesDynamicRegistration(maybeDynamicRegistration)
        setTraceSetting(params.getTrace)

        cancelChecker.checkCanceled()

        new InitializeResult(serverCapabilities())
    }

  override def initialized(params: InitializedParams): Unit =
    runSync {
      logger.debug("Client initialized")
      registerClientCapabilities()
      // Invoke initial compilation. Trigger it as build file changed.
      triggerInitialBuild()
    }

  /** Register needed capabilities with the client */
  def registerClientCapabilities(): Unit =
    if (state.clientAllowsWatchedFilesDynamicRegistration) {
      logger.debug("Register watched files")
      getClient()
        .register(clientCapabilities())
        .whenComplete {
          case (_, error) =>
            if (error != null)
              logger.error("Failed to register watched files", error)
        }
      ()
    } else {
      logger.debug("Client doesn't support dynamic registration for watched files")
    }

  override def didOpen(params: DidOpenTextDocumentParams): Unit =
    runSync {
      val fileURI = uri(params.getTextDocument.getUri)
      if (isFileScheme(fileURI)) {
        val code = Option(params.getTextDocument.getText)

        logger.debug(s"didOpen. fileURI: $fileURI. code.isDefined: ${code.isDefined}")

        didChangeAndPublish(
          fileURI = fileURI,
          code = code
        )
      }
    }

  override def didChange(params: DidChangeTextDocumentParams): Unit =
    runSync {
      val fileURI = uri(params.getTextDocument.getUri)
      if (isFileScheme(fileURI)) {
        val code = Option(params.getContentChanges.get(0).getText)

        logger.debug(s"didChange. fileURI: $fileURI. code.isDefined: ${code.isDefined}")

        didChangeAndPublish(
          fileURI = fileURI,
          code = code
        )
      }
    }

  override def didClose(params: DidCloseTextDocumentParams): Unit =
    runSync {
      val fileURI = uri(params.getTextDocument.getUri)

      if (isFileScheme(fileURI)) {
        logger.debug(s"didClose. fileURI: $fileURI")

        didChangeAndPublish(
          fileURI = fileURI,
          code = None
        )
      }
    }

  override def didSave(params: DidSaveTextDocumentParams): Unit =
    runSync {
      val fileURI = uri(params.getTextDocument.getUri)
      if (isFileScheme(fileURI)) {
        val code = Option(params.getText)

        logger.debug(s"didSave. fileURI: $fileURI. code.isDefined: ${code.isDefined}")

        didChangeAndPublish(
          fileURI = fileURI,
          code = code
        )
      }
    }

  override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit =
    runSync {
      val changes =
        params.getChanges.asScala.to(ArraySeq)

      logger.debug(s"didChangeWatchedFiles: ${changes.mkString("\n", "\n", "")}")

      // Check if GIT branch was changed
      if (changes.exists(_.getUri.endsWith(".git/HEAD"))) {
        // TODO: Support other version control systems.
        logger.debug("Branch change detected")
        reboot()
      } else {
        // collect events
        val events =
          changes
            .map {
              event =>
                (event.getType, event)
            }
            .collect {
              case (FileChangeType.Deleted, event) =>
                WorkspaceFileEvent.Deleted(uri(event.getUri))

              case (FileChangeType.Created, event) =>
                WorkspaceFileEvent.Created(uri(event.getUri))
            }

        val fileURIEvents =
          events filter {
            event =>
              isFileScheme(event.uri)
          }

        if (fileURIEvents.nonEmpty) {
          val currentPCState =
            getPCState()

          // Build OK! process delete or create
          val newPCState =
            PC.deleteOrCreate(
              events = fileURIEvents,
              pcState = currentPCState
            )

          // Set the updated workspace
          val diagnostics =
            setPCStateAndBuildDiagnostics(
              currentPCState = currentPCState,
              newPCState = newPCState
            )

          getClient() publish diagnostics
        }
      }

    }

  override def completion(params: CompletionParams): CompletableFuture[messages.Either[util.List[CompletionItem], CompletionList]] =
    runAsync {
      cancelChecker =>
        val fileURI = uri(params.getTextDocument.getUri)
        if (!isFileScheme(fileURI)) {
          messages.Either.forLeft(util.Arrays.asList())
        } else {
          val line      = params.getPosition.getLine
          val character = params.getPosition.getCharacter

          cancelChecker.checkCanceled()

          getPCState().workspace match {
            case sourceAware: WorkspaceState.IsSourceAware =>
              val completionResult =
                CodeProvider.search[Suggestion](
                  line = line,
                  character = character,
                  fileURI = fileURI,
                  workspace = sourceAware
                )

              val suggestions =
                completionResult match {
                  case Some(Right(suggestions)) =>
                    // completion successful
                    suggestions

                  case Some(Left(error)) =>
                    // Completion failed: Log the error message
                    logger.info("Code completion unsuccessful: " + error.message)
                    Iterator.empty[Suggestion]

                  case None =>
                    // Not a ralph file or it does not belong to the workspace's contract-uri directory.
                    Iterator.empty[Suggestion]
                }

              val completionList =
                CompletionConverter.toCompletionList(suggestions)

              cancelChecker.checkCanceled()

              messages.Either.forRight(completionList)

            case _: WorkspaceState.Created =>
              // Workspace must be compiled at least once to enable code completion.
              // The server must've invoked the initial compilation in the boot-up initialize function.
              logger.info("Code completion unsuccessful: Workspace is not compiled")
              messages.Either.forLeft(util.Arrays.asList())
          }
        }
    }

  override def definition(params: DefinitionParams): CompletableFuture[messages.Either[util.List[_ <: Location], util.List[_ <: LocationLink]]] =
    runAsync {
      cancelChecker =>
        val fileURI = uri(params.getTextDocument.getUri)
        if (!isFileScheme(fileURI)) {
          messages.Either.forLeft(util.Arrays.asList())
        } else {
          val line      = params.getPosition.getLine
          val character = params.getPosition.getCharacter

          cancelChecker.checkCanceled()

          getPCState().workspace match {
            case sourceAware: WorkspaceState.IsSourceAware =>
              // Can provide GoTo definition.
              val goToResult =
                CodeProvider.search[SourceLocation.GoTo](
                  line = line,
                  character = character,
                  fileURI = fileURI,
                  workspace = sourceAware
                )

              val locations =
                goToResult match {
                  case Some(Right(goToLocations)) =>
                    // successful
                    GoToConverter.toLocations(goToLocations)

                  case Some(Left(error)) =>
                    // Go-to definition failed: Log the error message
                    logger.info("Go-to definition unsuccessful: " + error.message)
                    Iterator.empty[Location]

                  case None =>
                    // Not a ralph file or it does not belong to the workspace's contract-uri directory.
                    Iterator.empty[Location]
                }

              cancelChecker.checkCanceled()

              messages.Either.forLeft(CollectionUtil.toJavaList(locations))

            case _: WorkspaceState.Created =>
              // Workspace must be compiled at least once to enable GoTo definition.
              // The server must've invoked the initial compilation in the boot-up initialize function.
              logger.info("Go-to definition unsuccessful: Workspace is not compiled")
              messages.Either.forLeft(util.Arrays.asList())
          }
        }
    }

  override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit =
    ()

  override def getTextDocumentService: TextDocumentService =
    this

  override def getWorkspaceService: WorkspaceService =
    this

  /**
   * Re-builds a fresh workspace from disk.
   */
  def reboot(): Unit =
    runSync {
      // initialise a new workspace
      val currentPCState =
        getPCState()

      val newPCState =
        PC.initialise(currentPCState.workspace.workspaceURI)

      // clear all existing diagnostics
      val diagnostics =
        setPCStateAndBuildDiagnostics(
          currentPCState = currentPCState,
          newPCState = newPCState
        )

      getClient() publish diagnostics

      // invoke initial build on new PCState
      triggerInitialBuild()
    }

  /**
   * Programmatically triggers a change in the build file.
   */
  private def triggerInitialBuild(): Unit =
    didChangeAndPublish(
      fileURI = getPCState().workspace.buildURI,
      code = None
    )

  /**
   * Apply code change and publish diagnostics.
   *
   * @param fileURI File that changed
   * @param code    Source-code of the changed file.
   */
  private def didChangeAndPublish(
      fileURI: URI,
      code: Option[String]): Unit =
    runSync {
      val diagnostics =
        didChangeAndSet(
          fileURI = fileURI,
          code = code
        )

      getClient() publish diagnostics
    }

  /**
   * Process code change and set the new workspace.
   *
   * @param fileURI File that changed.
   * @param code    Source-code of the changed file.
   * @return Diagnostics of the new workspace.
   */
  private def didChangeAndSet(
      fileURI: URI,
      code: Option[String]): Iterable[PublishDiagnosticsParams] =
    runSync {
      val currentPCState =
        getPCState()

      val newPCState =
        PC.changed(
          fileURI = fileURI,
          code = code,
          pcState = currentPCState
        )

      setPCStateAndBuildDiagnostics(
        currentPCState = currentPCState,
        newPCState = newPCState
      )
    }

  private def getPCState(): PCState =
    state.pcState getOrElse {
      // Workspace folder is not defined.
      // This is not expected to occur since `initialized` is always invoked first.
      notifyAndThrow(ResponseError.WorkspaceFolderNotSupplied)
    }

  private def getClient(): RalphLangClient =
    state.client getOrElse {
      val error     = ResponseError.ClientNotConfigured
      val exception = error.toResponseErrorException
      logger.error(error.getMessage, exception)
      throw exception
    }

  private def setPCState(pcState: PCState): Unit =
    runSync {
      thisServer.state = thisServer.state.copy(pcState = Some(pcState))
      ()
    }

  private def setClientAllowsWatchedFilesDynamicRegistration(allows: Boolean): Unit =
    runSync {
      thisServer.state = thisServer.state.copy(clientAllowsWatchedFilesDynamicRegistration = allows)
    }

  /** Store client configure trace setting. */
  private def setTraceSetting(traceValue: String): Unit =
    runSync {
      Trace(traceValue) match {
        case Left(error) =>
          getClient() show error

        case Right(trace) =>
          thisServer.state = thisServer.state.copy(trace = trace)
      }
      ()
    }

  /**
   * Set the workspace and returns diagnostics to publish for current state.
   *
   * @param newPCState Compilation result returned by presentation-compiler.
   * @return Diagnostics for current workspace.
   */
  private def setPCStateAndBuildDiagnostics(
      currentPCState: PCState,
      newPCState: Either[ErrorUnknownFileType, Option[PCState]]): Iterable[PublishDiagnosticsParams] =
    runSync {
      newPCState match {
        case Right(Some(newPCState)) =>
          setPCStateAndBuildDiagnostics(
            currentPCState = currentPCState,
            newPCState = newPCState
          )

        case Right(None) =>
          logger.debug("No server change occurred")
          Iterable.empty

        case Left(ErrorUnknownFileType(fileURI)) =>
          // Means: This fileURI does not belong to this workspace or is of different type.
          // If this occurs, it's a client configuration error.
          // File types that are not supported by Ralph should not be submitted to this server.
          notifyAndThrow(ResponseError.UnknownFileType(fileURI))
      }
    }

  /**
   * Set the new [[PCState]] and returns diagnostics to publish for the current state.
   *
   * @param currentPCState The [[PCState]] that got used to run this compilation.
   * @param newPCState     Compilation result returned by presentation-compiler.
   *                       [[None]] indicates that the file-type does not belong to us.
   * @return Diagnostics for current workspace.
   */
  private def setPCStateAndBuildDiagnostics(
      currentPCState: PCState,
      newPCState: PCState): Iterable[PublishDiagnosticsParams] =
    runSync {
      setPCState(newPCState)

      // build diagnostics for this PCState change
      val pcDiagnostics =
        Diagnostics.toFileDiagnostics(
          currentState = currentPCState,
          newState = newPCState
        )

      // convert the diagnostics to LSP4J types
      DiagnosticsConverter.toPublishParams(pcDiagnostics)
    }

  /** Write to log file, notify the client and throw to exit this request */
  private def notifyAndThrow(error: server.ResponseError): Nothing = {
    val client    = getClient()
    val exception = client.show(error).toResponseErrorException
    logger.error(error.getMessage, exception)
    throw exception
  }

  /** Write to log file and send the error to the client */
  private def logAndSend[A](error: server.ResponseError): CompletableFuture[A] = {
    logger.error(error.getMessage, error.toResponseErrorException)
    val result = new CompletableFuture[A]()
    result.completeExceptionally(error.toResponseErrorException)
    result
  }

  /**
   * Run within a synchronised block and ensure all unexpected abrupt internal errors are logged.
   */
  private def runSync[A](f: => A): A =
    try
      thisServer.synchronized {
        f
      }
    catch {
      case throwable: Throwable =>
        state.client match {
          case Some(client) =>
            // client is known, notify them.
            logger.error("Internal error occurred", throwable)
            client show ResponseError.InternalError(throwable)

          case None =>
            // client is not known.
            logger.error("Internal error occurred. Client not notified.", throwable)
        }

        throw throwable
    }

  private def runAsync[A](f: CancelChecker => A): CompletableFuture[A] =
    CompletableFutures
      .computeAsync(f(_))
      .whenComplete {
        case (_, error) =>
          if (error != null)
            logger.error("Async request failed", error)
      }

  override def setTrace(params: SetTraceParams): Unit =
    setTraceSetting(params.getValue)

  override def shutdown(): CompletableFuture[AnyRef] =
    runSync {
      logger.info("shutdown")
      if (thisServer.state.shutdownReceived) {
        logAndSend(ResponseError.ShutdownRequested)
      } else {
        thisServer.state = thisServer.state.copy(shutdownReceived = true)
        CompletableFuture.completedFuture(java.lang.Boolean.TRUE)
      }
    }

  def exitWithCode(): Int =
    runSync {
      logger.info("exit")

      thisServer.state.listener match {
        case Some(listener) =>
          listener.cancel(true)

        case None =>
          logger.error("Listener is empty. Exit invoked on server that is not initialised")
      }

      if (thisServer.state.shutdownReceived) {
        0
      } else {
        1
      }
    }

  override def exit(): Unit =
    System.exit(exitWithCode())

}
