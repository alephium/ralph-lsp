package org.alephium.ralph.lsp.pc.workspace.build.dependency.downloader

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.pc.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorDownloadingDependency

import java.net.URI
import java.nio.file.{FileSystems, Files, Path, Paths}
import scala.io.Source
import scala.jdk.CollectionConverters.{IteratorHasAsScala, MapHasAsJava}
import scala.util.{Failure, Success, Using}

object StdInterfaceDownloader extends StrictImplicitLogging {

  val stdFolder = "std"

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
  def stdInterfaces(dependencyPath: Path,
                    errorIndex: SourceIndex)(implicit logger: ClientLogger): Either[ErrorDownloadingDependency, List[SourceCodeState.UnCompiled]] =
    Using.Manager { use =>
      val stdURL = getClass.getResource(s"/$stdFolder")

      val stdPath = if (stdURL.getProtocol == "file") {
        Paths.get(stdURL.toURI)
      } else {
        // When using file from jar, the file as a special path
        val Array(jar, folder) = stdURL.toString.split("!")
        use(FileSystems.newFileSystem(URI.create(jar), Map[String, String]().asJava)).getPath(folder)
      }

      val interfaceFiles = use(Files.list(stdPath)).iterator().asScala.toList

      interfaceFiles.map { file =>
        val code = use(Source.fromInputStream(Files.newInputStream(file), "UTF-8")).getLines().mkString("\n")
        val filePath = dependencyPath.resolve(Paths.get(stdFolder).resolve(file.getFileName.toString))
        SourceCodeState.UnCompiled(
          fileURI = filePath.toUri,
          code = code
        )
      }
    } match {
      case Success(map) =>
        Right(map)

      case Failure(throwable) =>
        val error =
          ErrorDownloadingDependency(
            dependencyID = StdInterfaceDownloader.stdFolder,
            throwable = throwable,
            index = errorIndex
          )

        logger.error(error.title, throwable)

        Left(error)
    }
}
