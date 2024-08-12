// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.access.compiler.message

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.util.StringUtil

import java.net.URI

object SourceIndexExtra {

  def zero(fileURI: URI): SourceIndex =
    SourceIndex(
      index = 0,
      width = 0,
      fileURI = Some(fileURI)
    )

  /**
   * Sending negative index to the client would be incorrect.
   * This set the index to be an empty range.
   *
   * This is a temporary solution until an AST is available for `ralph.json`.
   *
   * @see Issue <a href="https://github.com/alephium/ralph-lsp/issues/17">#17</a>.
   */
  def ensurePositive(
      index: Int,
      width: Int,
      fileURI: URI): SourceIndex =
    if (index < 0)
      zero(fileURI)
    else
      new SourceIndex(
        index = index,
        width = width,
        fileURI = Some(fileURI)
      )

  /**
   * Similar to `String.lastIndexOf`, this returns the last index of
   * the given token in the provided code.
   *
   * @param token   The token to find.
   * @param code    The code within which to search for the token.
   * @param fileURI The file URI of the input code.
   * @return A [[SourceIndex]] if the token was found, otherwise the [[SourceIndex]]
   *         for the zeroth index.
   */
  def lastIndexOf(
      token: String,
      code: String,
      fileURI: URI): SourceIndex =
    SourceIndexExtra.ensurePositive(
      index = code.lastIndexOf(token), // TODO: lastIndexOf is temporary solution until an AST is available.
      width = token.length,
      fileURI = fileURI
    )

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

    /** Convert [[SourceIndex]] that contains index information to [[LineRange]] that contains line and character information */
    def toLineRange(code: String): LineRange =
      StringUtil.buildLineRange(code, sourceIndex.from, sourceIndex.to)

  }

}
