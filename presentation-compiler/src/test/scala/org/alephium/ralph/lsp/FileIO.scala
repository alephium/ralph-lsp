package org.alephium.ralph.lsp

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import org.scalatest.matchers.should.Matchers._

/**
 * Convenient File IO functions for test-cases.
 *
 * No effect handling required for test-cases.
 * Exceptions are OK for test-cases.
 * */
object FileIO {

  def writeBytes(bytes: Array[Byte],
                 uri: URI): Path = {
    //convert URI to Path
    val filePath = Paths.get(uri)
    // ensure directories exists
    createDirectories(filePath.getParent)
    //write to file
    Files.write(filePath, bytes)
  }

  def write(string: String,
            uri: URI): Path =
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

  def delete(path: Path): Unit =
    Files.delete(path)

  def exists(uri: URI): Boolean =
    Files.exists(Paths.get(uri))

}
