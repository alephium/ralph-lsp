package org.alephium.ralph.lsp.access.compiler.message

object SourceIndex {

  /** Empty range */
  val zero: SourceIndex =
    SourceIndex(
      from = 0,
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
      SourceIndex.zero
    else
      SourceIndex(
        from = index,
        width = width
      )
}

/**
 * @param from  Source position within the program.
 * @param width Width of targeted token/code trailing the `index`.
 */

final case class SourceIndex(from: Int, width: Int) {
  def to: Int =
    from + width

  /** Checks if the given index is within this SourceIndex's from and to index */
  def contains(index: Int): Boolean =
    index >= from && index <= to

  /** Offset this SourceIndex */
  def +(right: Int): SourceIndex =
    copy(from + right)
}
