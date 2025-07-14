// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp

import org.alephium.ralph.error.CompilerError
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.build.BuildState
import org.scalacheck.Gen
import org.scalatest.OptionValues._
import org.scalatest.matchers.should.Matchers.fail

import scala.reflect.ClassTag

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
  def castOrPrintErrors[T <: SourceCodeState](state: SourceCodeState)(implicit classTag: ClassTag[T]): T =
    state match {
      case error: SourceCodeState.IsError =>
        error match {
          case parseError: SourceCodeState.IsParserOrCompilationError =>
            val error =
              parseError
                .errors
                .map {
                  error =>
                    val compilerError = CompilerError.Default(error.message, Some(error.index))
                    compilerError.toFormatter(parseError.code).format(Some(Console.RED))
                }
                .mkString("\n")

            fail(s"""Actual: ${parseError.getClass}. Expected: ${classTag.runtimeClass}
                 |$error""".stripMargin)

          case access: SourceCodeState.ErrorAccess =>
            fail(s"""Actual: ${access.getClass}. Expected: ${classTag.runtimeClass}
                 |${Console.RED}${access.getClass}${Console.RESET}: ${access.error.message}""".stripMargin)
        }

      case result =>
        result.asInstanceOf[T]
    }

  /**
   * Tries to cast the given [[BuildState]] to the expected subtype [[T]].
   * If the state is an error, all contained errors are printed in a readable format for debugging.
   *
   * @param state The source code state to cast or inspect for errors.
   * @tparam T The type to cast to.
   * @return The casted [[BuildState]] if successful.
   */
  def castOrPrintErrors[T <: BuildState](state: BuildState)(implicit classTag: ClassTag[T]): T =
    state match {
      case BuildState.Errored(_, codeOption, errors, dependencies, _) =>
        // The build errored, print formatted errors for debugging.
        errors foreach {
          error =>
            val compilerError = CompilerError.Default(error.message, Some(error.index))
            codeOption match {
              case Some(code) =>
                val message = compilerError.toFormatter(code).format(Some(Console.RED))

                fail(s"""Actual: ${state.getClass}. Expected: ${classTag.runtimeClass}
                     |$message""".stripMargin)

              case None =>
                fail(s"Cannot print formatted error because `codeOption == None`. Error: $error")
            }
        }

        dependencies foreach {
          workspace =>
            workspace.sourceCode foreach castOrPrintErrors[SourceCodeState]
        }

        state.asInstanceOf[T]

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
          // For readability sort by index, so the error outputs are in the order of their position
          code = code.toList.sortBy(_.index.map(_.index))
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
        val index = {
          val index = result.index.value
          // If the width is `0`, ensure it is at least `1` so that the marker `^` is displayed in the error message.
          if (index.width <= 0)
            index.addToWidth(1)
          else
            index
        }

        val error          = CompilerError(message, Some(index))
        val formattedError = error.toFormatter(result.parsed.code).format(Some(Console.RED))
        s"Index: $index\n$formattedError"
    }

}
