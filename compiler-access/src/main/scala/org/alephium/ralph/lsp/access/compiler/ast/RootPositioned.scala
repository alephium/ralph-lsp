package org.alephium.ralph.lsp.access.compiler.ast

import org.alephium.ralph.Ast.Positioned

/** Default root [[Node]] */
case object RootPositioned extends Positioned {
  // The range is maximised so `node.sourceIndex.contains(index)` passes the root node.
  // The `endIndex` could be computed from children, but at the moment that computation is unnecessary.
  super.atSourceIndex(fromIndex = 0, endIndex = Int.MaxValue)
}
