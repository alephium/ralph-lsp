package org.alephium.ralph.lsp.server

import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.pc.workspace.build.BuildState

import java.util.concurrent.{Future => JFuture}

/**
 * Stores Ralph's LSP server state.
 *
 * @param client      Client proxy.
 * @param listener    Request listener which is canceled on server termination.
 * @param workspace   Current workspace state.
 * @param buildErrors Stores current build file errors which are not tied to the current workspace state.
 *                    - On successful build compilation, this gets set to [[None]] and the workspace is updated with the new build file.
 *                    - If there are build errors, the workspace still handles requests using the most recent valid and compiled build file
 *                      until this build file is error-free and successfully compiled.
 */
protected case class ServerState(client: Option[RalphLangClient],
                                 listener: Option[JFuture[Void]],
                                 workspace: Option[WorkspaceState],
                                 buildErrors: Option[BuildState.BuildErrored])
