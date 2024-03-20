package org.alephium.ralph.lsp.access.util

import org.alephium.ralph.lsp.access.compiler.message.{LinePosition, LineRange}
import org.scalatest.Assertions.fail

object TestStringUtil {
 def codeLines(code: String): Array[String] =
    code.split("\r\n|\r|\n")

 /**
   * Extracts line ranges from the code provided between the symbols `>>...<<`.
   *
   * @param code The code containing `>>...<<` symbols.
   * @return The line ranges, the code without the `>>...<<` symbols and the start/end indexes
   *
   */
 def lineRanges(code: String): (Array[LineRange], String, Int, Int) = {
    val lines = codeLines(code)
        .zipWithIndex

    val goToStart = lines.filter(_._1.contains(">>"))
    val goToEnd = lines.filter(_._1.contains("<<"))

    val start = code.indexOf(">>")
    val end = code.replace(">>","").indexOf("<<")

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
}
