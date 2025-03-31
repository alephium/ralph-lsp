// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp

import org.alephium.ralph.error.CompilerError
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation
import org.scalacheck.Gen
import org.scalatest.OptionValues._

/**
 * Common test data generator used by all other data types.
 */
object TestCommon {

  /** A random name. Restricted to 10 characters. */
  val genName: Gen[String] =
    Gen.listOfN(10, Gen.alphaChar).map(_.mkString)

  val genCamelCase: Gen[String] =
    genName.map(_.capitalize)

  /**
   * Some test error outputs are difficult to debug because [[org.alephium.ralph.SourceIndex]] only emits numbers.
   *
   * For example:
   * {{{
   *   LineRange(LinePosition(3, 16), LinePosition(3, 26)))) did not contain the same elements as Array()
   * }}}
   *
   * This function prints a formatted compiler error message for better test debuggability.
   *
   * @param codeBeingTested The actual code being tested, that contains the test markers `@@` and `>><<`.
   * @param code            The search result.
   */
  def tryOrPrintIndexer[T](
      codeBeingTested: Iterable[String],
      code: Iterable[SourceLocation.GoTo],
      message: String = "Actual"
    )(f: => T): T =
    try
      f
    catch {
      case throwable: Throwable =>
        // print the code was tested
        codeBeingTested foreach println

        // print the actual result
        printAsError(
          message = message,
          code = code
        )

        throw throwable
    }

  /**
   * Prints the given go-to search result as an error messages.
   *
   * @param message The pointer error message.
   * @param code    The search result.
   * @return String formatted error messages.
   */
  private def printAsError(
      message: String,
      code: Iterable[SourceLocation.GoTo]): Unit =
    toErrorMessage(
      message = message,
      code = code
    ).foreach(println)

  /**
   * Transforms the given go-to search result as an error messages.
   *
   * @param message The pointer error message.
   * @param code    The search result.
   * @return String formatted error messages.
   */
  private def toErrorMessage(
      message: String,
      code: Iterable[SourceLocation.GoTo]): Iterable[String] =
    code map {
      result =>
        val index          = result.index.value
        val error          = CompilerError(message, Some(index))
        val formattedError = error.toFormatter(result.parsed.code).format(Some(Console.RED))
        s"Index: $index\n$formattedError"
    }

}
