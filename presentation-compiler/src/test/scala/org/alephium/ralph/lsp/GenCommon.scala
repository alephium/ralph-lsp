package org.alephium.ralph.lsp

import org.alephium.ralph.lsp.compiler.CompilerAccess.RALPH_FILE_EXTENSION
import org.alephium.ralph.lsp.compiler.message.{error, CompilerMessage, SourceIndex}
import org.alephium.ralph.lsp.compiler.message.error.StringError
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
        folders.foldLeft(Option.empty[Path]) {
          case (path, folder) =>
            path
              .map(_.resolve(folder))
              .orElse(Some(Paths.get(folder)))
        }.get
    }
  }

  /** Generates a file name with the extension. Optionally creates a nested folder path. */
  def genFilePath(ext: String = RALPH_FILE_EXTENSION,
                  rootFolder: Gen[Option[Path]] = Gen.option(genFolder()),
                  fileFolder: Gen[Option[Path]] = Gen.option(genFolder())): Gen[Path] =
    for {
      fileName <- genName.map(fileName => s"$fileName.$ext")
      rootFolder <- rootFolder
      fileFolder <- fileFolder
    } yield {
      // append fileName to file path
      val filePath = fileFolder.map(_.resolve(fileName)).getOrElse(Paths.get(fileName))
      // append file path to folder path
      rootFolder.map(_.resolve(filePath)).getOrElse(filePath)
    }

  def genFileURI(ext: String = RALPH_FILE_EXTENSION,
                 rootFolder: Gen[Option[Path]] = Gen.option(genFolder()),
                 fileFolder: Gen[Option[Path]] = Gen.option(genFolder())): Gen[URI] =
    genFilePath(
      ext = ext,
      rootFolder = rootFolder,
      fileFolder = fileFolder
    ).map(_.toUri)

  /** Generate an error for this code */
  def genError(code: Gen[String] = genCode): Gen[CompilerMessage.AnyError] =
    for {
      code <- code
      errorMessage <- Gen.alphaStr
      errorIndex <- Gen.choose(0, code.length - 1)
    } yield
      error.StringError(
        message = errorMessage,
        index = SourceIndex(0, errorIndex) // TODO: gen random index location
      )

  def genErrors(code: String): Gen[List[CompilerMessage.AnyError]] =
    Gen.listOf(genError(Gen.const(code)))

  def genFolderAndFileURIs(): Gen[(URI, List[URI])] =
    for {
      folder <- genFolder()
      files <- Gen.listOf(genFileURI(rootFolder = Gen.const(Some(folder))))
    } yield
      (folder.toUri, files)

}
