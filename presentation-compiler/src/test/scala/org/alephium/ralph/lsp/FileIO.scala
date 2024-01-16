package org.alephium.ralph.lsp

import org.scalatest.matchers.should.Matchers._

import java.io.File
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/**
 * Convenient File IO functions for test-cases.
 *
 * No effect handling required for test-cases.
 * Exceptions are OK for test-cases.
 * */
object FileIO {

  def writeBytes(uri: URI,
                 bytes: Array[Byte]): Path = {
    //convert URI to Path
    val filePath = Paths.get(uri)
    // ensure directories exists
    createDirectories(filePath.getParent)
    //write to file
    Files.write(filePath, bytes)
  }

  def write(uri: URI,
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

  /** Recursive delete all files in this folder */
  def deleteAll(uri: URI): Unit =
    deleteAll(new File(uri))

  /** Recursive delete all files in this folder */
  private def deleteAll(file: File): Unit = {
    Option(file.listFiles()).foreach(_.foreach(deleteAll))
    file.delete()
  }

  def delete(path: Path): Unit =
    Files.delete(path)

  def exists(uri: URI): Boolean =
    Files.exists(Paths.get(uri))

}
