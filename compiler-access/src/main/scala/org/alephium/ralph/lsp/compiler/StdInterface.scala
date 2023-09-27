package org.alephium.ralph.lsp.compiler

import java.util.stream.Collectors
import java.io.{ File}
import java.net.{URL, URI}
import java.nio.file.{ Files,FileSystem, FileSystems, Path, Paths}
import scala.collection.JavaConverters._
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Success, Try, Using}
import com.typesafe.scalalogging.StrictLogging

object StdInterface {

  val stdFolder = "std"

  val stdPath = getPath(getClass.getResource(s"/$stdFolder"))

  val interfaceFiles = Files.list(stdPath).iterator().asScala.toList

  val stdInterfaces = interfaceFiles.map{file =>
    (s"$stdFolder/${removeExtension(file.getFileName.toString)}", readFile(file))
  }

  //https://stackoverflow.com/a/73045690
  private def getPath(url: URL): Path = {
    //When working with sbt and for test, std/interface are simple file
    if (url.getProtocol == "file"){
      Paths.get(url.toURI)
    } else {
      // When using file from jar, the file as a special path
      val Array(jar, folder) = url.toString.split("!")
      val jarFS = FileSystems.newFileSystem(URI.create(jar), Map[String, String]().asJava)
      jarFS.getPath(folder)
    }
  }

  private def readFile(path: Path): String ={
    Source.fromInputStream(Files.newInputStream(path), "UTF-8").getLines.mkString("\n")
  }


  private def removeExtension(fname: String): String = {
    val pos = fname.lastIndexOf('.');
    if(pos > -1)
      return fname.substring(0, pos);
    else
      return fname;
  }
}
