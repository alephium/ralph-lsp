package org.alephium.ralph.lsp.pc.sourcecode.imports

import java.net.URI
import java.nio.file.{ Files, FileSystems, Paths}
import scala.collection.JavaConverters._
import scala.io.Source
import scala.util.{Failure, Success, Using}
import com.typesafe.scalalogging.StrictLogging

object StdInterface extends StrictLogging {

  private val stdFolder = "std"

  /*
   * Get std interfaces, when using sbt it reads it from disk, when using with the jar
   * it reads them directly from the jar.
   * Currently if files can't be read, we stop the app otherwise we can't do much, but
   * as files are included in jar file on build, there's no reason it should happen.
   *
   * Later on we could improve and recover if we notice issue
   *
   * Interfaces are kept in memory as there aren't much, if in the future it start to have
   * to much data, we could think of reading them on the fly.
   *
   * We rely here on `Using` https://www.scala-lang.org/api/2.13.6/scala/util/Using$.html
   * to handle resources.
   */
  val stdInterfaces: Map[String, String] =
    Using.Manager { use =>
      val stdURL = getClass.getResource(s"/$stdFolder")

      val stdPath = if (stdURL.getProtocol == "file"){
        Paths.get(stdURL.toURI)
      } else {
        // When using file from jar, the file as a special path
        val Array(jar, folder) = stdURL.toString.split("!")
        use(FileSystems.newFileSystem(URI.create(jar), Map[String, String]().asJava)).getPath(folder)
      }

      val interfaceFiles = use(Files.list(stdPath)).iterator().asScala.toList

      interfaceFiles.map { file =>
        val code = use(Source.fromInputStream(Files.newInputStream(file), "UTF-8")).getLines.mkString("\n")
        (s"$stdFolder/${removeExtension(file.getFileName.toString)}", code)
      }.toMap
  } match {
    case Success(value) => value
    case Failure(error) =>
      logger.error(s"Cannot get std interfaces: $error")
      Map.empty
  }


  private def removeExtension(fname: String): String = {
    val pos = fname.lastIndexOf('.');
    if(pos > -1)
      return fname.substring(0, pos);
    else
      return fname;
  }
}
