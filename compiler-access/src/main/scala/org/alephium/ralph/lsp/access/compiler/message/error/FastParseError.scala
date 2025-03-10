// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.message.error

import fastparse.Parsed
import org.alephium.ralph.error.CompilerError
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage

object FastParseError {

  @inline def apply(failure: Parsed.Failure): FastParseError =
    FastParseError(CompilerError.FastParseError(failure))

}

/**
 * Stores error produced by `FastParse`.
 *
 * [[CompilerError.FastParseError]] also contains other error data, such as [[CompilerError.FastParseError.tracedMsg]]
 * which can be used for better error reports to the client.
 */
case class FastParseError(error: CompilerError.FastParseError) extends CompilerMessage.FormattedError
