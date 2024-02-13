package org.alephium.ralph.lsp.access.compiler.message

object SourceIndex {

  /** Empty range */
  val empty: SourceIndex =
    SourceIndex(
      index = 0,
      width = 0
    )

  /**
   * Sending negative index to the client would be incorrect.
   * This set the index to be an empty range.
   *
   * This is a temporary solution until an AST is available for `ralph.json`.
   *
   * @see Issue <a href="https://github.com/alephium/ralph-lsp/issues/17">#17</a>.
   */
  def ensurePositive(index: Int, width: Int): SourceIndex =
    if (index < 0)
      SourceIndex.empty
    else
      SourceIndex(
        index = index,
        width = width
      )
}

/**
 * @param index Source position within the program.
 * @param width Width of targeted token/code trailing the `index`.
 */

final case class SourceIndex(index: Int, width: Int) { self =>
  def toIndex =
    self.index + width

  /** Checks if the given index is within this SourceIndex's from and to index */
  def contains(index: Int): Boolean =
    index >= self.index && index <= toIndex

  /** Offset this SourceIndex */
  def +(right: Int): SourceIndex =
    copy(self.index + right)
}
