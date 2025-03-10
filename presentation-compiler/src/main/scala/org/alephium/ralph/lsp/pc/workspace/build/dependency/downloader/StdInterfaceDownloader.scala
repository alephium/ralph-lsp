// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace.build.dependency.downloader

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.pc.sourcecode.SourceCodeState
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID
import org.alephium.ralph.lsp.pc.workspace.build.error.ErrorDownloadingDependency

import java.net.URI
import java.nio.file.{Path, FileSystems, Paths, Files}
import scala.collection.immutable.ArraySeq
import scala.io.Source
import scala.jdk.CollectionConverters.{IteratorHasAsScala, MapHasAsJava}
import scala.util.{Using, Success, Failure}

object StdInterfaceDownloader extends DependencyDownloader.Native with StrictImplicitLogging {

  override def dependencyID: DependencyID.Std.type =
    DependencyID.Std

  /**
   * Download the Std package and return an un-compiled workspace for compilation.
   *
   * @param errorIndex Use this index to report any errors processing the download.
   */
  protected def _download(
      dependencyPath: Path,
      errorIndex: SourceIndex
    )(implicit logger: ClientLogger): Either[ArraySeq[CompilerMessage.AnyError], WorkspaceState.UnCompiled] =
    build(
      dependencyPath = dependencyPath,
      errorIndex = errorIndex
    ) match {
      case Right(interfacesSource) =>
        val workspaceDir =
          dependencyPath resolve dependencyID.dirName

        // a default build file.
        val build =
          DependencyDownloader.defaultBuild(workspaceDir)

        val state =
          WorkspaceState.UnCompiled(
            build = build,
            sourceCode = interfacesSource.to(ArraySeq)
          )

        Right(state)

      case Left(error) =>
        Left(ArraySeq(error))
    }

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
  private def build(
      dependencyPath: Path,
      errorIndex: SourceIndex
    )(implicit logger: ClientLogger): Either[ErrorDownloadingDependency, List[SourceCodeState.UnCompiled]] =
    Using.Manager {
      use =>
        val stdURL = getClass.getResource(s"/${dependencyID.dirName}")

        val stdPath = if (stdURL.getProtocol == "file") {
          Paths.get(stdURL.toURI)
        } else {
          // When using file from jar, the file as a special path
          val Array(jar, folder) = stdURL.toString.split("!")
          use(FileSystems.newFileSystem(URI.create(jar), Map[String, String]().asJava)).getPath(folder)
        }

        val interfaceFiles = use(Files.list(stdPath)).iterator().asScala.toList

        interfaceFiles.map {
          file =>
            val code     = use(Source.fromInputStream(Files.newInputStream(file), "UTF-8")).getLines().mkString("\n")
            val filePath = dependencyPath.resolve(Paths.get(dependencyID.dirName).resolve(file.getFileName.toString))
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
            dependencyID = dependencyID.dirName,
            throwable = throwable,
            index = errorIndex
          )

        logger.error(error.title, throwable)

        Left(error)
    }

}
