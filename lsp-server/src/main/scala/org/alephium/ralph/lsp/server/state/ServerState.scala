package org.alephium.ralph.lsp.server.state

import org.alephium.ralph.lsp.pc.PCState
import org.alephium.ralph.lsp.server.RalphLangClient

import java.util.concurrent.{Future => JFuture}

/**
 * Stores Ralph's LSP server state.
 *
 * @param client   Client proxy.
 * @param listener Request listener which is canceled on server termination.
 * @param pcState  Presentation-compiler state
 * @param trace    Client configured setting. See also [[org.eclipse.lsp4j.TraceValue]].
 */
case class ServerState(
    client: Option[RalphLangClient],
    listener: Option[JFuture[Void]],
    pcState: Option[PCState],
    clientAllowsWatchedFilesDynamicRegistration: Boolean,
    trace: Trace,
    shutdownReceived: Boolean)
