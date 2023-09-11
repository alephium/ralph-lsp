package org.alephium.ralph.lsp.pc.config

import org.alephium.ralph.error.CompilerError
import org.alephium.ralph.error.CompilerError.FormattableError
import org.alephium.ralph.lsp.compiler.CompilerAccess.RALPH_FILE_EXTENSION
import org.scalacheck.Gen

import java.net.URI
import java.nio.file.{Path, Paths}

/**
 * Common test data generator used by all other data types.
 */
object GenCommon {

  /** A random name. Restricted to 10 characters. */
  val genName: Gen[String] =
    Gen.listOfN(10, Gen.alphaChar).map(_.mkString)

  /** Generate ralph code */
  val genCode: Gen[String] =
    Gen.nonEmptyListOf(Gen.alphaChar).map(_.mkString) // TODO: Generate actual sourceCode

  /** Generate ralph code */
  def genFolder(nested: Boolean = true): Gen[Path] = {
    val folders =
      if (nested)
        Gen.nonEmptyListOf(genName)
      else
        Gen.listOfN(1, genName)

    folders map {
      folders =>
        folders.foldLeft(Paths.get("")) {
          case (path, folder) =>
            path.resolve(folder)
        }
    }
  }

  /** Generates a file name with the extension. Optionally creates a nested folder path. */
  def genFilePath(ext: String = RALPH_FILE_EXTENSION,
                  nestedFolders: Boolean = true): Gen[Path] =
    for {
      fileName <- genName.map(fileName => s"$fileName.$ext")
      folders <- genFolder(nestedFolders)
    } yield
      folders.resolve(fileName)

  def genFileURI(ext: String = RALPH_FILE_EXTENSION,
                 nestedFolders: Boolean = true): Gen[URI] =
    genFilePath(
      ext = ext,
      nestedFolders = nestedFolders
    ).map(_.toUri)

  /** Generate an error for this code */
  def genError(code: Gen[String] = genCode): Gen[FormattableError] =
    for {
      code <- code
      errorMessage <- Gen.alphaStr
      errorIndex <- Gen.choose(0, code.length - 1)
    } yield
      CompilerError.`Invalid number`(errorMessage, errorIndex)

  def genErrors(code: String): Gen[List[FormattableError]] =
    Gen.listOf(genError(Gen.const(code)))

}
