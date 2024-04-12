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
