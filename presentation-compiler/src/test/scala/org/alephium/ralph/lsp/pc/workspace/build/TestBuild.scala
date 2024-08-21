// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.GenExtension.GenExtensionsImplicits
import org.alephium.ralph.lsp.TestFile.genFolderURI
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.sourcecode.{TestSourceCode, SourceCodeState}
import org.alephium.ralph.lsp.pc.workspace.build.TestRalphc.genRalphcParsedConfig
import org.alephium.ralph.lsp.pc.workspace.build.config.{RalphcConfigState, RalphcConfig}
import org.alephium.ralph.lsp.pc.workspace.build.dependency.downloader.DependencyDownloader
import org.alephium.ralph.lsp.{TestCode, TestFile}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers._

import java.net.URI
import java.nio.file.Paths
import scala.collection.immutable.ArraySeq

/** Build specific generators */
object TestBuild {

  def genParsed(
      workspaceURI: Gen[URI] = genFolderURI(),
      config: Gen[RalphcConfigState.Parsed] = genRalphcParsedConfig()): Gen[BuildState.Parsed] =
    for {
      workspaceURI <- workspaceURI
      parsedConfig <- config
    } yield {
      val buildURI  = Build.toBuildFile(workspaceURI)
      val buildJSON = RalphcConfig.write(parsedConfig)
      //      FileIO.write(buildJSON, buildURI).toUri shouldBe buildURI
      // run either one of the two parse functions
      Build.parse(buildURI, buildJSON) match {
        case parsed: BuildState.Parsed =>
          parsed

        case errored: BuildState.Errored =>
          errored shouldBe a[BuildState.Parsed]
          fail("Expected parse to be successful")
      }
    }

  /** Generate a successfully compiled BuildState */
  def genCompiledOK(
      workspaceURI: Gen[URI] = genFolderURI(),
      config: Gen[RalphcConfigState.Parsed] = genRalphcParsedConfig(),
      dependencyDownloaders: ArraySeq[DependencyDownloader] = DependencyDownloader.all()
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): Gen[BuildState.Compiled] =
    genCompiled(
      workspaceURI = workspaceURI,
      config = config,
      dependencyDownloaders = dependencyDownloaders
    ).map(_.asInstanceOf[BuildState.Compiled])

  def genCompiled(
      workspaceURI: Gen[URI] = genFolderURI(),
      config: Gen[RalphcConfigState.Parsed] = genRalphcParsedConfig(),
      dependencyDownloaders: ArraySeq[DependencyDownloader] = DependencyDownloader.all()
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): Gen[BuildState.IsCompiled] =
    genParsed(
      workspaceURI = workspaceURI,
      config = config
    ) map {
      parsed =>
        Build.compile(
          parsed = persist(parsed),
          currentBuild = None,
          dependencyDownloaders = dependencyDownloaders
        )
    }

  /** Generate a successfully compiled build file with source-code inside and also outside the workspace */
  def genCompiledWithSourceCodeInAndOut(
      workspaceURI: Gen[URI] = genFolderURI(),
      code: Gen[String] = TestCode.genGoodCode(),
      minSourceCount: Int = 0,
      maxSourceCount: Int = 10,
      config: Gen[RalphcConfigState.Parsed] = genRalphcParsedConfig()
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): Gen[(BuildState.Compiled, List[SourceCodeState.OnDisk], List[SourceCodeState.OnDisk])] =
    for {
      // a compiled OK build file.
      (buildCompiled, workspaceSourceCode) <-
        genCompiledWithSourceCode(
          workspaceURI = workspaceURI,
          minSourceCount = minSourceCount,
          maxSourceCount = maxSourceCount,
          config = config
        )
      // write source-code files that are not in the workspace (expect these to get filtered out)
      outsideSourceCode <-
        Gen
          .listOfRange(minSourceCount, maxSourceCount)(TestSourceCode.genOnDisk())
          .map(TestSourceCode.persistAll(_, code))
    } yield (buildCompiled, workspaceSourceCode, outsideSourceCode)

  /** Generate a successfully compiled build file with source-code inside the workspace */
  def genCompiledWithSourceCode(
      workspaceURI: Gen[URI] = genFolderURI(),
      code: Gen[String] = TestCode.genGoodCode(),
      minSourceCount: Int = 0,
      maxSourceCount: Int = 10,
      config: Gen[RalphcConfigState.Parsed] = genRalphcParsedConfig()
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): Gen[(BuildState.Compiled, List[SourceCodeState.OnDisk])] =
    for {
      // a compiled OK build file.
      buildCompiled <- TestBuild.genCompiledOK(workspaceURI = workspaceURI, config = config)
      // write source-code files to build's contract path
      workspaceSourceCode <-
        Gen
          .listOfRange(minSourceCount, maxSourceCount)(TestSourceCode.genOnDiskForRoot(buildCompiled.contractURI))
          .map(TestSourceCode.persistAll(_, code))
    } yield (buildCompiled, workspaceSourceCode)

  def persist(parsed: BuildState.Parsed): BuildState.Parsed = {
    TestFile.write(parsed.buildURI, parsed.code)

    val workspacePath = Paths.get(parsed.workspaceURI)
    TestFile.createDirectories(workspacePath.resolve(parsed.config.contractPath))

    parsed.config.artifactPath foreach {
      artifactPath =>
        TestFile.createDirectories(workspacePath.resolve(artifactPath))
    }

    parsed
      .config
      .dependencyPath
      .foreach {
        path =>
          TestFile.createDirectories(workspacePath.resolve(path))
      }

    parsed
  }

  def persist(compiled: BuildState.Compiled): BuildState.Compiled = {
    TestFile.write(compiled.buildURI, compiled.code)

    val workspacePath = Paths.get(compiled.workspaceURI)
    TestFile.createDirectories(workspacePath.resolve(compiled.config.contractPath))

    compiled.config.artifactPath foreach {
      artifactPath =>
        TestFile.createDirectories(workspacePath.resolve(artifactPath))
    }

    TestFile.createDirectories(workspacePath.resolve(compiled.dependencyPath))

    compiled
  }

  def genExtendedContract(
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): Gen[(BuildState.Compiled, SourceCodeState.OnDisk, SourceCodeState.OnDisk, String, String)] =
    for {
      build                                <- TestBuild.genCompiledOK()
      (contract, extension, extensionName) <- TestCode.genExtendedContract()
      contractFile                         <- TestSourceCode.genOnDiskForRoot(rootURI = Gen.const(build.contractURI))
      extensionFile                        <- TestSourceCode.genOnDiskForRoot(rootURI = Gen.const(build.contractURI))
    } yield {
      val contractOnDisk  = TestSourceCode.persist(contractFile, code = Gen.const(contract))
      val extensionOnDisk = TestSourceCode.persist(extensionFile, code = Gen.const(extension))

      (build, contractOnDisk, extensionOnDisk, extension, extensionName)
    }

  /** Deletes the build file located at `.ralph-lsp/ralph.json`. */
  def deleteFile(build: BuildState): URI = {
    val fileURI = build.buildURI
    TestFile delete fileURI
    fileURI
  }

  /** Deletes the directory `.ralph-lsp` along with the build file `ralph.json`. */
  def deleteDirectory(build: BuildState): URI = {
    deleteFile(build)
    val directory = Build.toBuildDir(build.workspaceURI)
    TestFile delete directory
    directory
  }

}
