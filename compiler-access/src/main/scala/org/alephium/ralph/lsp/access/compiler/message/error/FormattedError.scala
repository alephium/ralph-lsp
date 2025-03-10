// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.message.error

import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage

/** Error type that can also output formatted message to console */
case class FormattedError(error: FormattableError) extends CompilerMessage.FormattedError
