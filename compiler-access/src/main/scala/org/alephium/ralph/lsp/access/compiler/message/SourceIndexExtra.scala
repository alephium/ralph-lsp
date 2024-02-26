package org.alephium.ralph.lsp.access.compiler.message

import org.alephium.ralph.SourceIndex

object SourceIndexExtra {

  val zero: SourceIndex =
    org.alephium.ralph.SourceIndex.empty

  implicit class SourceIndexExtension(val sourceIndex: SourceIndex) extends AnyVal {
    def from: Int =
      sourceIndex.index

    def to: Int =
      sourceIndex.endIndex

    def width: Int =
      sourceIndex.width

    /** Checks if the given index is within this SourceIndex's from and to index */
    def contains(index: Int): Boolean =
      index >= from && index <= to

    /** Offset this SourceIndex */
    def +(right: Int): SourceIndex =
      sourceIndex.copy(index = from + right)
  }

  implicit class SourceIndexTypeExtension(val sourceIndex: SourceIndex.type) extends AnyVal {

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
        zero
      else
        new SourceIndex(
          index = index,
          width = width
        )
  }
}
