package org.alephium.ralph.lsp

import org.alephium.ralph.lsp.access.compiler.CompilerAccess.RALPH_FILE_EXTENSION
import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndex}
import org.alephium.ralph.lsp.access.compiler.message.error.StringError
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

  val genCamelCase: Gen[String] =
    genName.map(_.capitalize)

  def genContract(name: Gen[String] = genCamelCase): Gen[String] =
    name map {
      name =>
        s"""
           |Contract $name(id:U256){
           |  pub fn getId() -> U256 {
           |    return id
           |  }
           |}
           |""".stripMargin
    }

  def genScript(name: Gen[String] = genCamelCase): Gen[String] =
    name map {
      name =>
        s"""
           |TxScript $name(x: U256, y: U256) {
           |  assert!(x != y, 0)
           |}
           |""".stripMargin
    }

  /** Generate ralph code */
  def genGoodCode(): Gen[String] =
    Gen.oneOf(
      genContract(genCamelCase),
      genScript(genCamelCase),
    )

  /** Generate ralph code */
  def genFolderPath(underTempDir: Boolean): Gen[Path] = {
    Gen.choose(2, 6) flatMap {
      maxNested =>
        val folders =
          Gen.listOfN(maxNested, genName)

        val tempDirectory: String =
          System.getProperty("java.io.tmpdir")

        val folderUnderTempDirectory =
          if (underTempDir)
            folders map {
              folders =>
                tempDirectory +: folders
            }
          else
            folders

        // generate folders ensuring they are children
        // of the same parent i.e. a root workspace directory.
        folderUnderTempDirectory map {
          folders =>
            Paths.get(folders.head, folders.tail: _*)
        }
    }
  }

  /** Generates a file name with the extension. Optionally creates a nested folder path. */
  def genFilePath(ext: String = RALPH_FILE_EXTENSION,
                  rootFolder: Gen[Path] = genFolderPath(underTempDir = true),
                  fileFolder: Gen[Option[Path]] = Gen.option(genFolderPath(underTempDir = false))): Gen[Path] =
    for {
      fileName <- genName.map(fileName => s"$fileName.$ext")
      rootFolder <- rootFolder
      fileFolder <- fileFolder
    } yield {
      // append fileName to file path
      val filePath = fileFolder.map(_.resolve(fileName)).getOrElse(Paths.get(fileName))
      // append file path to folder path
      rootFolder.resolve(filePath)
    }

  def genFileURI(ext: String = RALPH_FILE_EXTENSION,
                 rootFolder: Gen[Path] = genFolderPath(underTempDir = true),
                 fileFolder: Gen[Option[Path]] = Gen.option(genFolderPath(underTempDir = false))): Gen[URI] =
    genFilePath(
      ext = ext,
      rootFolder = rootFolder,
      fileFolder = fileFolder
    ).map(_.toUri)

  def genFolderURI(underTempDir: Boolean = true): Gen[URI] =
    genFolderPath(underTempDir = underTempDir).map(_.toUri)

  /** Generate an error for this code */
  def genError(code: Gen[String] = genGoodCode()): Gen[CompilerMessage.AnyError] =
    for {
      code <- code
      errorMessage <- Gen.alphaStr
      errorIndex <- Gen.choose(0, code.length - 1)
    } yield
      StringError(
        message = errorMessage,
        index = SourceIndex(0, errorIndex) // TODO: gen random index location
      )

  def genErrors(code: String): Gen[List[CompilerMessage.AnyError]] =
    Gen.listOf(genError(Gen.const(code)))
}
