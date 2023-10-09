package org.alephium.ralph.lsp.compiler.message

object SourceIndex {

  /** Empty range */
  val empty: SourceIndex =
    SourceIndex(
      index = 0,
      width = 0
    )
}

/** Source location as understood by FastParse.
 *
 * @param index
 * Source position within the program.
 * @param width
 * Width of targeted target token/code trailing the `index`.
 */

final case class SourceIndex(index: Int, width: Int)
