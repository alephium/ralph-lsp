package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.pc.workspace.build.TestRalphc.genRalphcParsedConfig
import org.alephium.ralph.lsp.pc.workspace.build.RalphcConfig.RalphcParsedConfig
import org.alephium.ralph.lsp.TestFile
import org.alephium.ralph.lsp.TestFile.genFolderURI
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers._

import java.net.URI

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
}
