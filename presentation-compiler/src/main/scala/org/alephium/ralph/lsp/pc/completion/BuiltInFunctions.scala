package org.alephium.ralph.lsp.pc.completion

import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorBuiltInFunctionsNotFound
import org.alephium.ralph.lsp.pc.util.PicklerUtil._

import java.net.URI
import java.nio.file.{Files, FileSystems, Paths}
import scala.io.Source
import scala.collection.JavaConverters._
import scala.util.Using
import com.typesafe.scalalogging.StrictLogging
import upickle.default._

object BuiltInFunctions  extends StrictLogging {

  private val json = "ralph-built-in-functions.json"

  def buildBuiltInFunctions: Either[CompilerMessage.AnyError, Seq[BuiltInFunction]] = Using.Manager { use =>
    val dataURL = getClass.getResource(s"/$json")

    val stdPath = if (dataURL.getProtocol == "file"){
      Paths.get(dataURL.toURI)
    } else {
      // When using file from jar, the file as a special path
      val Array(jar, folder) = dataURL.toString.split("!")
      use(FileSystems.newFileSystem(URI.create(jar), Map[String, String]().asJava)).getPath(folder)
    }

    val data = use(Source.fromInputStream(Files.newInputStream(stdPath), "UTF-8")).getLines.mkString("\n")

    read[Seq[BuiltInFunction]](data)

    }.toEither.left.map { error =>
      logger.error(s"Cannot get $json: $error")
      ErrorBuiltInFunctionsNotFound
    }
}
