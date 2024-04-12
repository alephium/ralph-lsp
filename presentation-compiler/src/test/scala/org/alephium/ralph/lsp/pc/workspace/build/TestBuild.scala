package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.GenExtension.GenExtensionsImplicits
import org.alephium.ralph.lsp.TestFile.genFolderURI
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.sourcecode.{TestSourceCode, SourceCodeState}
import org.alephium.ralph.lsp.pc.workspace.build.RalphcConfig.RalphcParsedConfig
import org.alephium.ralph.lsp.pc.workspace.build.TestRalphc.genRalphcParsedConfig
import org.alephium.ralph.lsp.{TestCode, TestFile}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers._

import java.net.URI

/** Build specific generators */
object TestBuild {

  def genParsed(
      workspaceURI: Gen[URI] = genFolderURI(),
      config: Gen[RalphcParsedConfig] = genRalphcParsedConfig()): Gen[BuildState.Parsed] =
    for {
      workspaceURI <- workspaceURI
      parsedConfig <- config
    } yield {
      val buildURI  = workspaceURI.resolve(Build.BUILD_FILE_NAME)
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
      config: Gen[RalphcParsedConfig] = genRalphcParsedConfig()
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): Gen[BuildState.Compiled] =
    genCompiled(
      workspaceURI = workspaceURI,
      config = config
    ).map(_.asInstanceOf[BuildState.Compiled])

  def genCompiled(
      workspaceURI: Gen[URI] = genFolderURI(),
      config: Gen[RalphcParsedConfig] = genRalphcParsedConfig()
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
          currentBuild = None
        )
    }

  /** Generate a successfully compiled build file with source-code inside and also outside the workspace */
  def genCompiledWithSourceCodeInAndOut(
      workspaceURI: Gen[URI] = genFolderURI(),
      code: Gen[String] = TestCode.genGoodCode(),
      minSourceCount: Int = 0,
      maxSourceCount: Int = 10
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): Gen[(BuildState.Compiled, List[SourceCodeState.OnDisk], List[SourceCodeState.OnDisk])] =
    for {
      // a compiled OK build file.
      (buildCompiled, workspaceSourceCode) <-
        genCompiledWithSourceCode(
          workspaceURI = workspaceURI,
          minSourceCount = minSourceCount,
          maxSourceCount = maxSourceCount
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
      maxSourceCount: Int = 10
    )(implicit file: FileAccess,
      compiler: CompilerAccess,
      logger: ClientLogger): Gen[(BuildState.Compiled, List[SourceCodeState.OnDisk])] =
    for {
      // a compiled OK build file.
      buildCompiled <- TestBuild.genCompiledOK(workspaceURI = workspaceURI)
      // write source-code files to build's contract path
      workspaceSourceCode <-
        Gen
          .listOfRange(minSourceCount, maxSourceCount)(TestSourceCode.genOnDiskForRoot(buildCompiled.contractURI))
          .map(TestSourceCode.persistAll(_, code))
    } yield (buildCompiled, workspaceSourceCode)

  def persist(parsed: BuildState.Parsed): BuildState.Parsed = {
    TestFile.write(parsed.buildURI, parsed.code)
    TestFile.createDirectories(parsed.workspaceURI.resolve(parsed.config.contractPath))
    TestFile.createDirectories(parsed.workspaceURI.resolve(parsed.config.artifactPath))
    parsed
      .config
      .dependencyPath
      .foreach {
        path =>
          TestFile.createDirectories(parsed.workspaceURI.resolve(path))
      }
    parsed
  }

  def persist(compiled: BuildState.Compiled): BuildState.Compiled = {
    TestFile.write(compiled.buildURI, compiled.code)
    TestFile.createDirectories(compiled.workspaceURI.resolve(compiled.config.contractPath.toUri))
    TestFile.createDirectories(compiled.workspaceURI.resolve(compiled.config.artifactPath.toUri))
    TestFile.createDirectories(compiled.workspaceURI.resolve(compiled.dependencyPath.toUri))
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

  def delete(build: BuildState): Unit =
    TestFile delete build.buildURI

}
