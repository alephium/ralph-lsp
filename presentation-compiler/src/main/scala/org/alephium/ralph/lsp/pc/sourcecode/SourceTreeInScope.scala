package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.access.compiler.ast.Tree

/**
 * Represents a single source tree ([[Tree.Source]]) within a source file ([[SourceCodeState.Parsed]]),
 * which can contain multiple source trees such as contracts, scripts etc.
 *
 * @param tree   The source tree within the parsed source file.
 * @param parsed The source file containing the source tree.
 */
case class SourceTreeInScope(
    tree: Tree.Source,
    parsed: SourceCodeState.Parsed)
