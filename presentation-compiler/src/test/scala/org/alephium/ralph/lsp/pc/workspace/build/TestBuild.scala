package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.TestFile
import org.alephium.ralph.lsp.TestFile.genFolderURI
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

  def genBuildParsed(workspaceURI: Gen[URI] = genFolderURI(),
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

  def persist(parsed: BuildState.BuildParsed): BuildState.BuildParsed = {
    TestFile.write(parsed.buildURI, parsed.code)
    TestFile.createDirectories(parsed.workspaceURI.resolve(parsed.config.contractPath))
    TestFile.createDirectories(parsed.workspaceURI.resolve(parsed.config.artifactPath))
    parsed
  }

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
}
