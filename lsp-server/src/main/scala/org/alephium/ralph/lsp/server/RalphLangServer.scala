// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.{PC, PCState, PCStates}
import org.alephium.ralph.lsp.pc.diagnostic.Diagnostics
import org.alephium.ralph.lsp.pc.search.MultiCodeProvider
import org.alephium.ralph.lsp.pc.search.completion.Suggestion
import org.alephium.ralph.lsp.pc.search.gotoref.multi.GoToRefMultiSetting
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.pc.workspace._
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorUnknownFileType
import org.alephium.ralph.lsp.server
import org.alephium.ralph.lsp.server.MessageMethods.{WORKSPACE_WATCHED_FILES, WORKSPACE_WATCHED_FILES_ID}
import org.alephium.ralph.lsp.server.converter.{CompletionConverter, DiagnosticsConverter, GoToConverter, RenameConverter}
import org.alephium.ralph.lsp.server.state.{ServerState, Trace}
import org.alephium.ralph.lsp.utils.{IsCancelled, ProcessUtil}
import org.alephium.ralph.lsp.utils.log.StrictImplicitLogging
import org.alephium.ralph.lsp.utils.URIUtil.{isFileScheme, uri}
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.{messages, CancelChecker, CompletableFutures}
import org.eclipse.lsp4j.services._

import java.net.URI
import java.nio.file.Paths
import java.util
import java.util.concurrent.{CompletableFuture, Future => JFuture}
import scala.collection.immutable.ArraySeq
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.IterableHasAsScala
import scala.jdk.FutureConverters.FutureOps

object RalphLangServer extends StrictImplicitLogging {

  /** Start server with pre-configured client */
  def apply(
      client: RalphLangClient,
      listener: JFuture[Void],
      clientAllowsWatchedFilesDynamicRegistration: Boolean = false
    )(implicit compiler: CompilerAccess,
      file: FileAccess,
      ec: ExecutionContext): RalphLangServer = {
    val initialState =
      ServerState(
        client = Some(client),
        listener = Some(listener),
        pcStates = PCStates.empty,
        clientAllowsWatchedFilesDynamicRegistration = clientAllowsWatchedFilesDynamicRegistration,
        trace = Trace.Off,
        shutdownReceived = false
      )

    new RalphLangServer(initialState)
  }

  // format: off
  def apply()(implicit compiler: CompilerAccess,
              file: FileAccess,
              ec: ExecutionContext): RalphLangServer = // format: on
    new RalphLangServer(
      ServerState(
        client = None,
        listener = None,
        pcStates = PCStates.empty,
        clientAllowsWatchedFilesDynamicRegistration = false,
        trace = Trace.Off,
        shutdownReceived = false
      )
    )

  def getWorkspaceFolders(params: InitializeParams): Option[Iterable[URI]] = {
    // fetch workspaces folders
    val folders =
      Option(params.getWorkspaceFolders) match {
        case Some(workspaceFolders) =>
          workspaceFolders.asScala map {
            folder =>
              URI.create(folder.getUri)
          }

        case None =>
          Iterable.empty
      }

    if (folders.isEmpty)
      // Some LSP clients aren't providing `rootUri` or `rootPath`, like in nvim, so we fall back on `user.dir`
      Option(System.getProperty("user.dir"))
        .map(Paths.get(_))
        .map(_.toUri)
        .map(Iterable(_))
    else
      Some(folders)
  }

  /** Build capabilities supported by the LSP server */
  def serverCapabilities(): ServerCapabilities = {
    val capabilities = new ServerCapabilities()

    capabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
    capabilities.setCompletionProvider(new CompletionOptions(false, util.Arrays.asList(".")))
    capabilities.setDefinitionProvider(true)
    capabilities.setReferencesProvider(true)
    capabilities.setRenameProvider(true)

    // Workspace Capabilities
    val workspaceCapabilities  = new WorkspaceServerCapabilities()
    val workspaceFolderOptions = new WorkspaceFoldersOptions()
    workspaceFolderOptions.setChangeNotifications(true)
    workspaceCapabilities.setWorkspaceFolders(workspaceFolderOptions)
    capabilities.setWorkspace(workspaceCapabilities)

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

  @inline private def toIsCancelled(cancelChecker: CancelChecker): IsCancelled =
    IsCancelled(cancelChecker.isCanceled)

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
    file: FileAccess,
    ec: ExecutionContext)
  extends LanguageServer
     with TextDocumentService
     with WorkspaceService
     with StrictImplicitLogging { thisServer =>

  import org.alephium.ralph.lsp.server.RalphLangServer._

  /**
   * If true, allows go-to-definition API to
   * use [[org.alephium.ralph.lsp.access.compiler.parser.soft.SoftParser]]
   * for more syntax-error tolerant responses.
   *
   * TODO: Moved to `ralph.json` as a build configuration.
   *       {{{
   *         "experimental": {
   *            "enableSoftParser": true
   *          }
   *       }}}
   */
  private val enableSoftParser: Boolean =
    true

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

        ProcessUtil.closeOnExit(
          processId = params.getProcessId,
          closeable = () => thisServer.exit()
        )

        // Previous commit uses the non-deprecated API but that does not work in vim.
        val workspaceFolderURIs =
          RalphLangServer.getWorkspaceFolders(params)

        val workspaceURIs =
          workspaceFolderURIs getOrElse notifyAndThrow(ResponseError.WorkspaceFolderNotSupplied)

        workspaceURIs foreach {
          workspaceURI =>
            putPCState(PC.initialise(workspaceURI))
        }

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
      // Invoke initial compilation. Trigger it as a build file changed.
      getPCStatesOrFail() foreach triggerInitialBuild
    }

  /**
   * Programmatically triggers a change request in the build files.
   *
   * Both build files get processed from disk.
   */
  def triggerInitialBuild(pcState: PCState): Unit = {
    // Trigger the build of `alephium.config.ts` first, which, if it exists, will generate `ralph.json`.
    didChangeAndPublish(
      fileURI = pcState.workspace.tsBuildURI,
      code = None
    )

    // If the above does not generate `ralph.json`, this will.
    // The cost here is relatively low because rebuilding `ralph.json` will be
    // cancelled immediately if it's already built by the above call.
    didChangeAndPublish(
      fileURI = getPCStateOrFail(pcState.workspace.workspaceURI).workspace.buildURI,
      code = None
    )
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

      val events =
        changes
          .collect {
            case fileEvent if isFileScheme(uri(fileEvent.getUri)) =>
              val fileURI = uri(fileEvent.getUri)

              fileEvent.getType match {
                case FileChangeType.Created =>
                  WorkspaceFileEvent.Created(fileURI)

                case FileChangeType.Changed =>
                  WorkspaceFileEvent.Changed(fileURI)

                case FileChangeType.Deleted =>
                  WorkspaceFileEvent.Deleted(fileURI)
              }
          }
          .groupBy {
            event =>
              getPCStateOrNone(event.uri)
          }

      events foreach {
        case (Some(currentPCState), events) =>
          if (events.nonEmpty) {
            // Build OK! process delete or create
            val newPCState =
              PC.events(
                events = events,
                pcState = currentPCState
              )

            // Set the updated workspace
            val diagnostics =
              putPCStateAndBuildDiagnostics(
                currentPCState = currentPCState,
                newPCState = newPCState
              )

            getClient() publish diagnostics
          }

        case (None, events) =>
          // Occurs when no workspace is found for an event's `fileURI`.
          // This can happen when modifying dependency files located under the home directory at `.ralph-lsp/dependencies/code.ral`.
          logger.trace(s"Cannot execute events (count = ${events.size}) because source file is not within an active workspace: '${events.map(_.uri).distinct.mkString(", ")}'")
      }
    }

  override def completion(params: CompletionParams): CompletableFuture[messages.Either[util.List[CompletionItem], CompletionList]] =
    runFuture {
      isCancelled =>
        MultiCodeProvider
          .search[Unit, Suggestion](
            fileURI = uri(params.getTextDocument.getUri),
            line = params.getPosition.getLine,
            character = params.getPosition.getCharacter,
            enableSoftParser = enableSoftParser,
            isCancelled = isCancelled,
            pcStates = getPCStates(),
            settings = ()
          )
          .map(_.map(CompletionConverter.toCompletionItemsEither))
    }

  override def definition(params: DefinitionParams): CompletableFuture[messages.Either[util.List[_ <: Location], util.List[_ <: LocationLink]]] =
    runFuture {
      isCancelled =>
        MultiCodeProvider
          .search[Unit, SourceLocation.GoToDef](
            fileURI = uri(params.getTextDocument.getUri),
            line = params.getPosition.getLine,
            character = params.getPosition.getCharacter,
            enableSoftParser = enableSoftParser,
            isCancelled = isCancelled,
            pcStates = getPCStates(),
            settings = ()
          )
          .map(_.map(GoToConverter.toLocationEither))
    }

  override def references(params: ReferenceParams): CompletableFuture[util.List[_ <: Location]] =
    runFuture {
      isCancelled =>
        MultiCodeProvider
          .search[GoToRefMultiSetting, SourceLocation.GoToRefStrict](
            fileURI = uri(params.getTextDocument.getUri),
            line = params.getPosition.getLine,
            character = params.getPosition.getCharacter,
            enableSoftParser = enableSoftParser,
            isCancelled = isCancelled,
            pcStates = getPCStates(),
            settings = GoToRefMultiSetting(params.getContext.isIncludeDeclaration)
          )
          .map(_.map(GoToConverter.toLocations))
    }

  override def rename(params: RenameParams): CompletableFuture[WorkspaceEdit] =
    runFuture {
      isCancelled =>
        MultiCodeProvider
          .search[Unit, SourceLocation.GoToRenameStrict](
            fileURI = uri(params.getTextDocument.getUri),
            line = params.getPosition.getLine,
            character = params.getPosition.getCharacter,
            enableSoftParser = enableSoftParser,
            isCancelled = isCancelled,
            pcStates = getPCStates(),
            settings = ()
          )
          .map(_.map(RenameConverter.toWorkspaceEdits(_, params.getNewName)))
    }

  override def didChangeWorkspaceFolders(params: DidChangeWorkspaceFoldersParams): Unit = {
    val added   = params.getEvent.getAdded.asScala.map(_.getUri).map(URI.create)
    val removed = params.getEvent.getRemoved.asScala.map(_.getUri).map(URI.create)

    addWorkspaceFolders(added)
    removeWorkspaceFolders(removed)
  }

  override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit =
    ()

  override def getTextDocumentService: TextDocumentService =
    this

  override def getWorkspaceService: WorkspaceService =
    this

  /**
   * Inserts and builds the new workspace folders.
   *
   * @param uris Root workspace directory URIs to be added.
   */
  private def addWorkspaceFolders(uris: Iterable[URI]): Unit =
    uris foreach {
      newURI =>
        // Initialise a new PCState
        val pcState = PC.initialise(newURI)
        // Insert the new initialised PCState
        putPCState(pcState)
        // Trigger initial build
        triggerInitialBuild(pcState)
    }

  /**
   * Removes the given workspace folder.
   *
   * @param uris Root workspace directory URIs to be removed.
   */
  private def removeWorkspaceFolders(uris: Iterable[URI]): Unit =
    uris foreach {
      removedURI =>
        // remove workspace from state
        removePCState(removedURI) match {
          case Some(removedPCStates) =>
            // publish an empty state to clear all existing diagnostics for the removed workspace.
            removedPCStates foreach {
              removed =>
                val diagnostics =
                  buildDiagnostics(
                    currentPCState = removed,
                    newPCState = PC.initialise(removedURI)
                  )

                // For quicker client updates, execute `publish` after each `build` instead of together.
                getClient() publish diagnostics
            }

          case None =>
            logger.error(s"Workspace-folder not found for URI $removedURI")
        }
    }

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
      getPCStateOrNone(fileURI) match {
        case Some(currentPCState) =>
          val newPCState =
            PC.changed(
              fileURI = fileURI,
              code = code,
              pcState = currentPCState
            )

          putPCStateAndBuildDiagnostics(
            currentPCState = currentPCState,
            newPCState = newPCState
          )

        case None =>
          logger.trace(s"Cannot execute change because the source file is not within an active workspace: '$fileURI'")
          Iterable.empty
      }
    }

  private def getPCStateOrFail(fileURI: URI): PCState =
    getPCStateOrNone(fileURI) getOrElse {
      // Workspace folder is not defined.
      // This is not expected to occur since `initialized` is always invoked first.
      notifyAndThrow(ResponseError.SourceNotInWorkspace(fileURI))
    }

  private def getPCStateOrNone(fileURI: URI): Option[PCState] =
    getPCStates().findContains(fileURI)

  private def getPCStatesOrFail(): ArraySeq[PCState] = {
    val pcStates = getPCStates()
    if (pcStates.states.isEmpty)
      // Workspace folder is not defined.
      // This is not expected to occur since `initialized` is always invoked first.
      notifyAndThrow(ResponseError.WorkspaceFolderNotSupplied)
    else
      pcStates.states
  }

  private def getPCStates(): PCStates =
    thisServer.state.pcStates

  private def removePCState(fileURI: URI): Option[ArraySeq[PCState]] =
    thisServer.state.remove(fileURI) map {
      case (newState, removedPCStates) =>
        thisServer.state = newState
        removedPCStates
    }

  private def getClient(): RalphLangClient =
    thisServer.state.client getOrElse {
      val error     = ResponseError.ClientNotConfigured
      val exception = error.toResponseErrorException
      logger.error(error.getMessage, exception)
      throw exception
    }

  private def putPCState(pcState: PCState): Unit =
    runSync {
      thisServer.state = thisServer.state.put(pcState)
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
  private def putPCStateAndBuildDiagnostics(
      currentPCState: PCState,
      newPCState: Either[ErrorUnknownFileType, Option[PCState]]): Iterable[PublishDiagnosticsParams] =
    runSync {
      newPCState match {
        case Right(Some(newPCState)) =>
          putPCStateAndBuildDiagnostics(
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
   * @return Diagnostics for current workspace.
   */
  private def putPCStateAndBuildDiagnostics(
      currentPCState: PCState,
      newPCState: PCState): Iterable[PublishDiagnosticsParams] =
    runSync {
      putPCState(newPCState)

      buildDiagnostics(
        currentPCState = currentPCState,
        newPCState = newPCState
      )
    }

  /**
   * Returns diagnostics to publish for the current state.
   *
   * @param currentPCState The [[PCState]] that got used to run this compilation.
   * @param newPCState     Compilation result returned by presentation-compiler.
   * @return Diagnostics for current workspace.
   */
  private def buildDiagnostics(
      currentPCState: PCState,
      newPCState: PCState): Iterable[PublishDiagnosticsParams] =
    runSync {
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

  /** Notify the client and log */
  private def notify(error: server.ResponseError): ResponseError = {
    val client    = getClient()
    val exception = client.show(error).toResponseErrorException
    logger.error(error.getMessage, exception)
    error
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

  /**
   * Runs a Scala [[Future]] within Java's [[CompletableFuture]].
   * If an error occurs, the client is notified.
   *
   * @param f The function to execute.
   */
  private def runFuture[A](f: IsCancelled => Future[Either[CompilerMessage.Error, A]]): CompletableFuture[A] =
    CompletableFutures
      .computeAsync {
        cancelChecker =>
          // Expect a scala future.
          // Scala `Future`s are used internally within `presentation-compiler`.
          val scalaFuture =
            f(toIsCancelled(cancelChecker)).flatMap {
              case Left(error) =>
                val responseError = ResponseError.InvalidRequest(error)
                notify(responseError)
                Future.failed(responseError.toResponseErrorException)

              case Right(result) =>
                Future.successful(result)
            }

          // convert back to Java
          scalaFuture.asJava
      }
      .thenCompose(java.util.function.Function.identity())
      .whenComplete {
        case (_, error) =>
          if (error != null)
            logger.error("Async request failed", error)
      }

  override def setTrace(params: SetTraceParams): Unit =
    setTraceSetting(params.getValue)

  override def shutdown(): CompletableFuture[AnyRef] =
    runSync {
      logger.info("Shutdown request received")
      if (thisServer.state.shutdownReceived) {
        logger.info("Shutdown already in progress")
        logAndSend(ResponseError.ShutdownRequested)
      } else {
        thisServer.state = thisServer.state.copy(shutdownReceived = true)
        CompletableFuture.completedFuture(java.lang.Boolean.TRUE)
      }
    }

  def exitWithCode(): Int =
    runSync {
      logger.info("System exit initiated")

      thisServer.state.listener match {
        case Some(listener) =>
          logger.trace("Cancelling listener")
          val result = listener.cancel(true)
          if (result)
            logger.trace("Listener cancelled")

        case None =>
          logger.error("Listener is empty. Exit invoked on server that is not initialised.")
      }

      val code =
        if (thisServer.state.shutdownReceived)
          0
        else
          1

      logger.info(s"Exit with code $code")
      code
    }

  override def exit(): Unit =
    System.exit(exitWithCode())

}
