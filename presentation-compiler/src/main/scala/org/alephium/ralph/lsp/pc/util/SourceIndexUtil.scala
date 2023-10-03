package org.alephium.ralph.lsp.pc.util

import org.alephium.ralph.SourceIndex

object SourceIndexUtil {

  implicit class SourceIndexExtensions(sourceIndex: SourceIndex) {

    /**
     * Sending negative index to the client would be incorrect.
     * This returns an error reporting the first character as error.
     *
     * This is a temporary solution until an AST is available for `build.ralph`.
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
