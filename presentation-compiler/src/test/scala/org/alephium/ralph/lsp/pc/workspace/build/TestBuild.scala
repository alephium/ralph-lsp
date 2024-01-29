package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.GenExtension.GenExtensionsImplicits
import org.alephium.ralph.lsp.TestFile
import org.alephium.ralph.lsp.TestFile.genFolderURI
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, TestSourceCode}
import org.alephium.ralph.lsp.pc.workspace.build.RalphcConfig.RalphcParsedConfig
import org.alephium.ralph.lsp.pc.workspace.build.TestRalphc.genRalphcParsedConfig
import org.alephium.ralph.lsp.pc.workspace.build.dependency.TestDependency
import org.scalacheck.Gen
import org.scalatest.OptionValues._
import org.scalatest.matchers.should.Matchers._

import java.net.URI
import java.nio.file.Paths

/** Build specific generators */
object TestBuild {

  def genParsed(workspaceURI: Gen[URI] = genFolderURI(),
                config: Gen[RalphcParsedConfig] = genRalphcParsedConfig()): Gen[BuildState.BuildParsed] =
    for {
      workspaceURI <- workspaceURI
      parsedConfig <- config
    } yield {
      val buildURI = workspaceURI.resolve(Build.BUILD_FILE_NAME)
      val buildJSON = RalphcConfig.write(parsedConfig)
      //      FileIO.write(buildJSON, buildURI).toUri shouldBe buildURI
      // run either one of the two parse functions
      Build.parse(buildURI, buildJSON) match {
        case parsed: BuildState.BuildParsed =>
          parsed

        case errored: BuildState.BuildErrored =>
          errored shouldBe a[BuildState.BuildParsed]
          fail("Expected parse to be successful")
      }
    }

  /** Generate a successfully compiled BuildState */
  def genCompiledOK(workspaceURI: Gen[URI] = genFolderURI(),
                    config: Gen[RalphcParsedConfig] = genRalphcParsedConfig())(implicit file: FileAccess,
                                                                               compiler: CompilerAccess,
                                                                               logger: ClientLogger): Gen[BuildState.BuildCompiled] =
    genCompiled(
      workspaceURI = workspaceURI,
      config = config
    ).map(_.asInstanceOf[BuildState.BuildCompiled])

  def genCompiled(workspaceURI: Gen[URI] = genFolderURI(),
                  config: Gen[RalphcParsedConfig] = genRalphcParsedConfig())(implicit file: FileAccess,
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

  /** Generate a successfully compiled build file with source-code inside the workspace and also outside */
  def genCompiledWithSourceCodeInAndOut(workspaceURI: Gen[URI] = genFolderURI())(implicit file: FileAccess,
                                                                                 compiler: CompilerAccess,
                                                                                 logger: ClientLogger): Gen[(BuildState.BuildCompiled, List[SourceCodeState.OnDisk], List[SourceCodeState.OnDisk])] =
    for {
      // a compiled OK build file.
      buildCompiled <- TestBuild.genCompiledOK(workspaceURI = workspaceURI)
      // write source-code files to build's contract path
      workspaceSourceCode <- Gen.listOfMax(5)(TestSourceCode.genOnDiskForRoot(buildCompiled.contractURI)).map(TestSourceCode.persistAll(_))
      // write source-code files that are not in the workspace (expect these to get filtered out)
      outsideSourceCode <- Gen.listOfMax(5)(TestSourceCode.genOnDisk()).map(TestSourceCode.persistAll(_))
    } yield (buildCompiled, workspaceSourceCode, outsideSourceCode)

  /**
   * Converts the Parsed build [[BuildState.BuildParsed]] to a Compiled build [[BuildState.BuildCompiled]].
   *
   * Does NOT actually compile the parsed build file.
   * If parse errors are expected, compile it with [[Build.compile]] instead.
   * */
  def toCompiled(build: BuildState.BuildParsed): BuildState.BuildCompiled =
    BuildState.BuildCompiled(
      buildURI = build.buildURI,
      code = build.code,
      dependency =
        Some(TestDependency.buildStd().dependency.value), // standard dependency must be defined
      config =
        org.alephium.ralphc.Config( // compiler configuration must use the paths from the build file
          compilerOptions = build.config.compilerOptions,
          contractPath = Paths.get(build.workspaceURI.resolve(build.config.contractPath)),
          artifactPath = Paths.get(build.workspaceURI.resolve(build.config.artifactPath)),
        )
    )


  def persist(parsed: BuildState.BuildParsed): BuildState.BuildParsed = {
    TestFile.write(parsed.buildURI, parsed.code)
    TestFile.createDirectories(parsed.workspaceURI.resolve(parsed.config.contractPath))
    TestFile.createDirectories(parsed.workspaceURI.resolve(parsed.config.artifactPath))
    parsed
  }

  def persist(compiled: BuildState.BuildCompiled): BuildState.BuildCompiled = {
    TestFile.write(compiled.buildURI, compiled.code)
    TestFile.createDirectories(compiled.workspaceURI.resolve(compiled.config.contractPath.toUri))
    TestFile.createDirectories(compiled.workspaceURI.resolve(compiled.config.artifactPath.toUri))
    compiled
  }
}
