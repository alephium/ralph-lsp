package org.alephium.ralph.lsp.compiler.message.error

import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.lsp.compiler.message.CompilerMessage

/** Error message that can also output formatted error message to console  */
case class FormattedError(error: FormattableError) extends CompilerMessage.FormattedError
