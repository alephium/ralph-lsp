// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace

import java.net.URI

case class WorkspaceFile(
    fileURI: URI,
    text: Option[String])
