package org.alephium.ralph.lsp.access.compiler.ast.node

import org.alephium.ralph.Ast.Positioned
import org.alephium.ralph.SourceIndex

/** Default root [[Node]]'s data. */
case class RootPosition(index: SourceIndex) extends Positioned {
  super.atSourceIndex(Some(index))
}
