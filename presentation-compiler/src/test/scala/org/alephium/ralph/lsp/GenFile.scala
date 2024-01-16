package org.alephium.ralph.lsp

import org.alephium.ralph.lsp.GenCommon._
import org.alephium.ralph.lsp.access.compiler.CompilerAccess.RALPH_FILE_EXTENSION
import org.scalacheck.Gen

import java.net.URI
import java.nio.file.{Path, Paths}

object GenFile {

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

}
