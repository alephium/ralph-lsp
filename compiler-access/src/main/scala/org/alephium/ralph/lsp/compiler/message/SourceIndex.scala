package org.alephium.ralph.lsp.compiler.message

object SourceIndex {

  /** Empty range */
  val empty: SourceIndex =
    SourceIndex(
      index = 0,
      width = 0
    )
}

/**
 * @param index Source position within the program.
 * @param width Width of targeted token/code trailing the `index`.
 */

final case class SourceIndex(index: Int, width: Int) {

  /**
   * Sending negative index to the client would be incorrect.
   * This set the index to be an empty range.
   *
   * This is a temporary solution until an AST is available for `build.ralph`.
   * See Issue <a href="https://github.com/alephium/ralph-lsp/issues/17">#17</a>.
   */
  def ensureNotNegative(): SourceIndex =
    if (index < 0)
      SourceIndex.empty
    else
      this
}
