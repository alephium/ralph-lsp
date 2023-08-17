package org.alephium.ralph.lsp.pc

import fastparse.Parsed
import org.alephium.ralph.{Ast, StatelessParser}
import org.alephium.ralph.error.CompilerError

/**
 * Implements functions for interactive programming in Ralph
 * accessing Ralph's core compiler.
 */
object PresentationCompiler {

  /**
   * Dumb compiler to test error reporting in IDE.
   *
   * TODO: Access actual Ralph compiler
   */
  def compile(code: String): Either[CompilerError.FormattableError, Ast.AssetScript] =
    try
      fastparse.parse(code, StatelessParser.assetScript(_)) match {
        case Parsed.Success(ast, _) =>
          Right[CompilerError.FormattableError, Ast.AssetScript](ast)

        case failure: Parsed.Failure =>
          Left(CompilerError.FastParseError(failure))
      }
    catch {
      case error: Error => // TODO: Error is untyped error data. This needs to be a typed error `FormattableError` with `SourcePosition` info.
        Left(CompilerError.`Invalid number`(error.getMessage, 0)) // Temporarily use another/random error type
    }

}
