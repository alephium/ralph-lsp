// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.hover

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}

private case object HoverDeclaration extends StrictImplicitLogging {

  /**
   * Prepares the hover content for a given declaration.
   *
   * @param declaration The declaration for which to prepare hover content.
   * @param sourceCode The source code state at the location of the hover request.
   * @return An optional hover content if the declaration type is supported.
   */
  def apply(
      declaration: SoftAST.DeclarationAST,
      sourceCode: SourceLocation.CodeSoft
    )(implicit logger: ClientLogger): Option[SourceLocation.Hover] =
    hoverDeclaration(declaration).map {
      content =>
        SourceLocation.Hover(
          content = content,
          code = sourceCode,
          initialAST = declaration
        )
    }

  /**
   * Extracts the hover content for a given AST declaration.
   *
   * This function checks the type of the declaration and returns the representation to use for hover,
   * if any is available for the declaration type.
   *
   * @param declaration The declaration for which to extract hover content.
   * @return An optional declaration representation to show on hover.
   */
  def hoverDeclaration(declaration: SoftAST.DeclarationAST)(implicit logger: ClientLogger): Option[SoftAST.DeclarationAST] =
    declaration match {
      case function: SoftAST.Function =>
        Some(hoverFunction(function))

      case other =>
        logger.error(s"Hover not implemented for declaration '${other.getClass.getSimpleName}' at source index '${other.index}'")
        None
    }

  /**
   * Returns a function declaration suitable for hover display.
   *
   * Removes the function body and any post-signature whitespace from the result
   * so only the function signature is shown.
   *
   * @param function The function declaration to process.
   * @return The function declaration with the uncessary parts removed for hover display.
   */
  private def hoverFunction(function: SoftAST.Function): SoftAST.Function =
    // TODO: Remove unnecessary spaces
    function.deepCopy(SourceIndex.empty).copy(block = None, postSignatureSpace = None)

}
