package org.alephium.ralph.lsp.pc.data

import org.eclipse.lsp4j.{InitializeResult, PublishDiagnosticsParams}

/**
 * Initial response from server to client.
 *
 * @param result      Capabilities currently supported by the server
 * @param diagnostics All initial project/workspace level compilation errors
 */
case class InitializeResponse(result: InitializeResult,
                              diagnostics: Option[PublishDiagnosticsParams])
