package org.alephium.ralph.lsp

import org.alephium.ralph.lsp.TestCommon._
import org.alephium.ralph.lsp.access.compiler.CompilerAccess.RALPH_FILE_EXTENSION
import org.scalacheck.Gen
import org.scalatest.TryValues._
import org.scalatest.matchers.should.Matchers._

import java.io.File
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Paths, Files}
import scala.io.Source
import scala.util.Using

/**
 * [[File]] IO related test functions
 *
 * No effect handling required for test-cases.
 * Exceptions are OK for test-cases.
 */
object TestFile {

  /**
   * ********************
   * Generators for Files
   * *********************
   */

  /** Generate ralph code */
  def genFolderPath(underTempDir: Boolean): Gen[Path] =
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

  /** Generates a file name with the extension. Optionally creates a nested folder path. */
  def genFilePath(
      ext: String = RALPH_FILE_EXTENSION,
      rootFolder: Gen[Path] = genFolderPath(underTempDir = true),
      fileFolder: Gen[Option[Path]] = Gen.option(genFolderPath(underTempDir = false))): Gen[Path] =
    for {
      fileName <-
        genName map {
          fileName =>
            s"$fileName.$ext"
        }
      rootFolder <- rootFolder
      fileFolder <- fileFolder
    } yield {
      // append fileName to file path
      val filePath = fileFolder.map(_.resolve(fileName)).getOrElse(Paths.get(fileName))
      // append file path to folder path
      rootFolder.resolve(filePath)
    }

  def genFileURI(
      ext: String = RALPH_FILE_EXTENSION,
      rootFolder: Gen[Path] = genFolderPath(underTempDir = true),
      fileFolder: Gen[Option[Path]] = Gen.option(genFolderPath(underTempDir = false))): Gen[URI] =
    genFilePath(
      ext = ext,
      rootFolder = rootFolder,
      fileFolder = fileFolder
    ).map(_.toUri)

  def genFolderURI(underTempDir: Boolean = true): Gen[URI] =
    genFolderPath(underTempDir = underTempDir).map(_.toUri)

  /**
   * *************************
   * File IO related functions
   * *************************
   */

  /** Write bytes to the URI */
  def writeBytes(
      uri: URI,
      bytes: Array[Byte]): Path = {
    // convert URI to Path
    val filePath = Paths.get(uri)
    // ensure directories exists
    createDirectories(filePath.getParent)
    // write to file
    Files.write(filePath, bytes)
  }

  def write(
      uri: URI,
      string: String): Path =
    writeBytes(
      uri = uri,
      bytes = string.getBytes(StandardCharsets.UTF_8)
    )

  def createDirectories(path: Path): Path = {
    val directory = Files.createDirectories(path)
    directory shouldBe path
    path
  }

  def createDirectories(uri: URI): Path =
    createDirectories(Paths.get(uri))

  def delete(uri: URI): Unit =
    Files.delete(Paths.get(uri))

  def deleteIfExists(uri: URI): Boolean =
    Files.deleteIfExists(Paths.get(uri))

  /** Recursive delete all files in this folder */
  def deleteAll(uri: URI): Boolean =
    deleteAll(new File(uri))

  /** Recursive delete all files in this folder */
  private def deleteAll(file: File): Boolean = {
    Option(file.listFiles()).foreach(_.foreach(deleteAll))
    file.delete()
  }

  def delete(path: Path): Unit =
    Files.delete(path)

  def exists(uri: URI): Boolean =
    Files.exists(Paths.get(uri))

  def readAll(uri: URI): String =
    Using(Source.fromFile(uri))(_.mkString)
      .success
      .value

}
