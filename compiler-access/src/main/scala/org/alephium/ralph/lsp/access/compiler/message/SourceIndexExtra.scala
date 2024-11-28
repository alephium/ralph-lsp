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
   * Creates a [[SourceIndex]] when the start and end indexes are known.
   *
   * @param from the starting index of the range (inclusive)
   * @param to   the ending index of the range (exclusive)
   */
  @inline def range(
      from: Int,
      to: Int): SourceIndex =
    SourceIndex(
      index = from,
      width = to - from,
      fileURI = None
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

  def contains(
      parent: Option[SourceIndex],
      child: Option[SourceIndex]): Boolean =
    (parent, child) match {
      case (Some(parent), Some(child)) =>
        parent contains child

      case (_, _) =>
        false
    }

  /**
   * Checks if the `current` position is before the `anchor`'s position.
   *
   * @param current The SourceIndex whose position is being tested.
   * @param anchor  The SourceIndex with which the position of `current` is compared.
   * @return `true` if `current`'s position is before `anchor`'s position, `false` otherwise.
   */
  def isBehind(
      current: Option[SourceIndex],
      anchor: Option[SourceIndex]): Boolean =
    current
      .zip(anchor)
      .exists {
        case (current, anchor) =>
          current isBehind anchor
      }

  /**
   * Checks if the `current` SourceIndex's position is after the `anchor` SourceIndex's position.
   *
   * @param current The SourceIndex whose position is being tested.
   * @param anchor  The SourceIndex with which the position of `current` is compared.
   * @return `true` if `current`'s position is after `anchor`'s position, `false` otherwise.
   */
  def isAhead(
      current: Option[SourceIndex],
      anchor: Option[SourceIndex]): Boolean =
    current
      .zip(anchor)
      .exists {
        case (current, anchor) =>
          current isAhead anchor
      }

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

    def contains(child: SourceIndex): Boolean =
      (sourceIndex.fileURI, child.fileURI) match {
        case (Some(parentURI), Some(childURI)) if parentURI == childURI =>
          sourceIndex contains child.from

        case (_, _) =>
          false
      }

    def isBehind(that: SourceIndex): Boolean =
      isBehind(that.from)

    def isBehind(index: Int): Boolean =
      sourceIndex.from < index

    def isAhead(that: SourceIndex): Boolean =
      isAhead(that.from)

    def isAhead(index: Int): Boolean =
      sourceIndex.from > index

    /** Offset this SourceIndex */
    def +(right: Int): SourceIndex =
      sourceIndex.copy(index = from + right)

    /** Convert [[SourceIndex]] that contains index information to [[LineRange]] that contains line and character information */
    def toLineRange(code: String): LineRange =
      StringUtil.buildLineRange(code, sourceIndex.from, sourceIndex.to)

  }

}
