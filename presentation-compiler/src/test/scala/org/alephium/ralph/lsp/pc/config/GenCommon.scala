package org.alephium.ralph.lsp.pc.config

import org.alephium.ralph.error.CompilerError
import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.lsp.compiler.CompilerAccess.RALPH_FILE_EXTENSION
import org.scalacheck.Gen

import java.net.URI
import java.nio.file.FileSystems

/**
 * Common test data generator used by all other data types.
 */
object GenCommon {

  val pathSeparator =
    FileSystems.getDefault.getSeparator

  /** a random name */
  val genName: Gen[String] =
    Gen.listOfN(10, Gen.alphaChar).map(_.mkString)

  // TODO: Generate actual sourceCode
  val genCode: Gen[String] =
    Gen.nonEmptyListOf(Gen.alphaChar).map(_.mkString)

  def genFolder(nested: Boolean = true): Gen[URI] = {
    val folders =
      if (nested)
        Gen.nonEmptyListOf(genName)
      else
        Gen.listOfN(1, genName)

    val path =
      folders.map(_.mkString(pathSeparator))

    path.map(URI.create)
  }

  /** Generates a file name with the extension. Optionally creates a nested folder path. */
  def genFile(ext: String = RALPH_FILE_EXTENSION,
              nested: Boolean = true): Gen[URI] =
    for {
      fileName <- genName.map(fileName => s"$fileName.$ext")
      folders <- genFolder(nested)
    } yield
      folders.resolve(fileName)

  /** Generate some error for this code */
  def genFormattableError(forCode: String): Gen[FormattableError] =
    for {
      errorMessage <- Gen.alphaStr
      errorIndex <- Gen.choose(0, forCode.length - 1)
    } yield
      CompilerError.`Invalid number`(errorMessage, errorIndex)

  def genFormattableErrors(forCode: String): Gen[List[FormattableError]] =
    Gen.listOf(genFormattableError(forCode))

}
