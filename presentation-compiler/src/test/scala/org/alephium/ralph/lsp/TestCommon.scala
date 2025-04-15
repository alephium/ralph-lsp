// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp

import org.alephium.ralph.error.CompilerError
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.scalacheck.Gen
import org.scalatest.OptionValues._
import org.scalatest.matchers.should.Matchers.fail

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
   * Tries to cast the given [[SourceCodeState]] to the expected subtype [[T]].
   * If the state is an error, all contained errors are printed in a readable format for debugging.
   *
   * @param state The source code state to cast or inspect for errors.
   * @tparam T The type to cast to.
   * @return The casted [[SourceCodeState]] if successful.
   */
  def castOrPrintErrors[T <: SourceCodeState](state: SourceCodeState): T =
    state match {
      case error: SourceCodeState.IsError =>
        error match {
          case parseError: SourceCodeState.IsParserOrCompilationError =>
            parseError.errors foreach {
              error =>
                val compilerError = CompilerError.Default(error.message, Some(error.index))
                val errorMessage  = compilerError.toFormatter(parseError.code).format(Some(Console.RED))
                println(errorMessage)
            }

            fail(s"Actual: ${parseError.getClass}. Expected: SourceCodeState.Parsed")

          case access: SourceCodeState.ErrorAccess =>
            println(s"${Console.RED}${access.getClass.getSimpleName}${Console.RESET}: ${access.error.message}")
            fail(s"Actual: ${access.getClass}. Expected: SourceCodeState.Parsed")
        }

      case result =>
        result.asInstanceOf[T]
    }

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
