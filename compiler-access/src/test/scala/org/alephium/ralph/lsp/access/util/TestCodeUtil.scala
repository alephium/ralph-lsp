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

package org.alephium.ralph.lsp.access.util

import org.alephium.ralph.lsp.access.compiler.message.{LinePosition, LineRange}
import org.scalatest.Assertions.fail

object TestCodeUtil {

  /** Use this in your test-case for */
  val SEARCH_INDICATOR =
    "@@"

  def codeLines(code: String): Array[String] =
    code.split("\r\n|\r|\n")

  /**
   * Extracts line ranges from the code provided between the symbols `>>...<<`.
   *
   * @param code The code containing `>>...<<` symbols.
   * @return The line ranges, the code without the `>>...<<` symbols and the start/end indexes
   */
  def lineRanges(code: String): (Array[LineRange], String, Int, Int) = {
    val lines = codeLines(code).zipWithIndex

    val goToStart = lines.filter(_._1.contains(">>"))
    val goToEnd   = lines.filter(_._1.contains("<<"))

    val start = code.indexOf(">>")
    val end   = code.replace(">>", "").indexOf("<<")

    val expectedLineRanges =
      if (goToStart.length != goToEnd.length)
        fail(s"Location indicators '>>' and '<<' not provided")
      else
        goToStart
          .zip(goToEnd)
          .map {
            case ((startLine, startLineIndex), (endLine, endLineIndex)) =>
              // Code range should be where >> and << are located
              LineRange(
                from = LinePosition(startLineIndex, startLine.indexOf(">>")),
                to = LinePosition(endLineIndex, endLine.replaceFirst(">>", "").indexOf("<<"))
              )
          }

    // remove << and >>
    val codeWithoutGoToSymbols =
      code.replaceAll(">>|<<", "")

    (expectedLineRanges, codeWithoutGoToSymbols, start, end)
  }

  /**
   *  Extracts the 'LinePosition', as well as the index from the code provided where `@@` is located.
   */
  def indicatorPosition(code: String): (LinePosition, Int, String) = {
    val lines = codeLines(code)

    // find the line where @@ is located
    lines.zipWithIndex.find(_._1.contains(SEARCH_INDICATOR)) match {
      case Some((line, lineIndex)) =>
        val index = code.indexOf(SEARCH_INDICATOR)
        // find the character where @@ is located
        val character =
          line.indexOf(SEARCH_INDICATOR)

        // remove @@
        val codeWithoutAtSymbol =
          code.replaceFirst(SEARCH_INDICATOR, "")

        val linePosition = LinePosition(lineIndex, character)

        (linePosition, index, codeWithoutAtSymbol)

      case None =>
        fail(s"Location indicator '$SEARCH_INDICATOR' not provided")
    }
  }

}
