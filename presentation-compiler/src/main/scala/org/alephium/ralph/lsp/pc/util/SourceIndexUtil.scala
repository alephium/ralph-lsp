package org.alephium.ralph.lsp.pc.util

import org.alephium.ralph.SourceIndex

object SourceIndexUtil {

  implicit class SourceIndexExtensions(sourceIndex: SourceIndex) {

    /**
     * Sending negative index to the client would be incorrect.
     * This set the index to be an empty range.
     *
     * This is a temporary solution until an AST is available for `build.ralph`.
     * See Issue <a href="https://github.com/alephium/ralph-lsp/issues/17">#17</a>.
     */
    def ensureNotNegative(): SourceIndex =
      if (sourceIndex.index < 0)
        SourceIndex(
          index = 0,
          width = 0
        )
      else
        sourceIndex
  }

}
