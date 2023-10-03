package org.alephium.ralph.lsp.pc.util

import org.alephium.ralph.SourceIndex

object SourceIndexUtil {

  implicit class SourceIndexExtensions(sourceIndex: SourceIndex) {

    /**
     * Sending negative index to the client is never correct.
     * This returns an error reporting the first character as error.
     *
     * This is a temporary solution for when issue until an AST is
     * available for build files.
     *
     * @return [[SourceIndex]] with a non-negative index
     */
    def ensureNotNegative(): SourceIndex =
      if (sourceIndex.index < 0)
        SourceIndex(
          index = 0,
          width = 1
        )
      else
        sourceIndex
  }

}
