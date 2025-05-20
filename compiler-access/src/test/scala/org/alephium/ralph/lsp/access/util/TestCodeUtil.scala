// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.util

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.{LinePosition, LineRange, SourceIndexExtra}
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.scalatest.Assertions.fail
import org.scalatest.matchers.should.Matchers._

import scala.collection.immutable.ArraySeq

object TestCodeUtil {

  /** Use this in your test-case for */
  val SEARCH_INDICATOR =
    "@@"

  def codeLines(code: String): Array[String] =
    code.split("\r\n|\r|\n")

  def lineRange(code: String): LineRange = {
    val (range, _, _, _) = lineRanges(code)
    range should have size 1
    range.head
  }

  def lineRangesOnly(code: String): Array[LineRange] =
    lineRanges(code)._1

  def removeRangeSymbols(code: String): String =
    lineRanges(code)._2

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
  def indicatorPositionOrFail(code: String): (LinePosition, Int, String) =
    // find the line where @@ is located
    indicatorPosition(code) getOrElse fail(s"Location indicator '$SEARCH_INDICATOR' not provided")

  // find the line where @@ is located
  def indicatorPosition(code: String): Option[(LinePosition, Int, String)] =
    codeLines(code)
      .zipWithIndex
      .find(_._1.contains(SEARCH_INDICATOR))
      .map {
        case (line, lineIndex) =>
          val index = code.indexOf(SEARCH_INDICATOR)
          // find the character where @@ is located
          val character =
            line.indexOf(SEARCH_INDICATOR)

          // remove @@
          val codeWithoutAtSymbol =
            code.replaceFirst(SEARCH_INDICATOR, "")

          val linePosition = LinePosition(lineIndex, character)

          (linePosition, index, codeWithoutAtSymbol)
      }

  /**
   * Extracts the [[SourceIndex]] from the given input.
   *
   * @param code the input string containing the tokens `>>` and `<<`
   * @return [[SourceIndex]] of the substring between the tokens `>>` and `<<`
   */
  def indexOf(code: String): SourceIndex =
    indexCodeOf(code)._1

  /**
   * Extracts the [[SourceIndex]] and the string chunk enclosing the tokens `>>` and `<<`.
   *
   * @param code the input string containing the tokens `>>` and `<<`
   * @return [[SourceIndex]] and the substring of the string between the tokens `>>` and `<<`.
   */
  def indexChunkOf(code: String): (SourceIndex, String) = {
    val (index, unmarkedCode) = indexCodeOf(code)
    val chunk                 = unmarkedCode.substring(index.from, index.to)
    (index, chunk)
  }

  /**
   * Extracts the [[SourceIndex]] for the substring between the tokens `>>` and `<<`
   * and returns a tuple containing the extracted [[SourceIndex]] and the code
   * with the `>>` and `<<` tokens removed.
   *
   * @param code String containing the tokens `>>` and `<<`.
   * @return A tuple where:
   *          - The first element is the [[SourceIndex]] of the substring between the tokens `>>` and `<<`.
   *          - The second element is the code with the `>>` and `<<` tokens removed.
   */
  def indexCodeOf(code: String): (SourceIndex, String) = {
    val start              = code.indexOf(">>")
    val codeWithoutStart   = code.replaceFirst(">>", "")
    val end                = codeWithoutStart.indexOf("<<")
    val codeWithoutSymbols = codeWithoutStart.replaceFirst("<<", "")
    (SourceIndexExtra.range(start, end), codeWithoutSymbols)
  }

  /**
   * Extracts the location of the `@@` marker from the given source code.
   * Only one `@@` marker is expected in the input. Any `>><<` markers will be removed.
   *
   * @param code The source code lines containing at most on `@@` marker.
   * @return A tuple of:
   *         - An optional pair of the `@@` marker's position and the cleaned line (without the markers).
   *         - Other source code with the search marker `>><<` removed.
   */
  def extractAtInfo(code: ArraySeq[String]): (Option[(LinePosition, String)], ArraySeq[String]) = {
    val (withAt, withoutAt) =
      code partitionMap {
        code =>
          val codeWithoutRangeMarkers = code.replaceAll(">>|<<", "")
          // - Left = Code with the `@@` marker (only one expected)
          // - Right = Code without the `@@` marker
          TestCodeUtil.indicatorPosition(codeWithoutRangeMarkers) match {
            case Some((location, _, code)) =>
              // persist the source with the `@@` marker.
              Left((location, code))

            case None =>
              Right(codeWithoutRangeMarkers)
          }
      }

    // there should only be one source-code with the `@@` symbol
    withAt.size should be <= 1
    (withAt.headOption, withoutAt)
  }

  /**
   * Extracts the line range `>><<` marker information from the given source code.
   * Only the `>><<` markers are expected in the input. Any `@@` markers will be removed.
   *
   * @param code The source code lines containing the `>><<` marker.
   * @return The `>><<` marker positions and the cleaned line (without the markers).
   */
  def extractLineRangeInfo(code: ArraySeq[String]): ArraySeq[(Array[LineRange], String)] = {
    val codeWithoutAtMarker =
      code.map(_.replace(TestCodeUtil.SEARCH_INDICATOR, ""))

    (codeWithoutAtMarker map TestCodeUtil.lineRanges).map {
      case (ranges, code, _, _) =>
        (ranges, code)
    }
  }

  /**
   * Given the following code string, extracts the text `": U256"` along with its range information.
   *
   * Example:
   * {{{
   *   @@
   *   Contract Test() {
   *     fn test() -> () {
   *       let one>>: U256<< = 1
   *     }
   *   }
   * }}}
   *
   * @param code The source code to analyse.
   * @return The line range and the extracted text.
   */
  def extractLineRangeTextInfo(code: String): Array[(LineRange, String)] = {
    // Remove `@@` markers
    val codeWithoutAtMarker =
      code.replace(TestCodeUtil.SEARCH_INDICATOR, "")

    // Extract all line range information `>><<` markers
    val (ranges, cleanCode, _, _) =
      TestCodeUtil.lineRanges(codeWithoutAtMarker)

    // Fetch the substring containing the line-ranges.
    ranges map {
      range =>
        val fromIndex =
          StringUtil.computeIndex(
            code = cleanCode,
            line = range.from.line,
            character = range.from.character
          )

        val toIndex =
          StringUtil.computeIndex(
            code = cleanCode,
            line = range.to.line,
            character = range.to.character
          )

        val rangeText = cleanCode.substring(fromIndex, toIndex)

        (range, rangeText)
    }
  }

  def clearTestMarkers(code: String): String =
    code.replaceAll(">>|<<|@@", "")

}
