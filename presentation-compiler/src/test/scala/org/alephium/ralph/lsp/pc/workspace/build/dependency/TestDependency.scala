package org.alephium.ralph.lsp.pc.workspace.build.dependency

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.FileClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.workspace.build.{BuildState, RalphcConfig}
import org.scalatest.matchers.should.Matchers._

import java.net.URI

object TestDependency {

  /** Build the standard library */
  def buildStd(): BuildState.BuildCompiled = {
    implicit val logger: ClientLogger =
      FileClientLogger

    implicit val file: FileAccess =
      null

    implicit val compiler: CompilerAccess =
      CompilerAccess.ralphc

    // create a default build file.
    val parsed =
      BuildState.BuildParsed(
        buildURI = URI.create("file:///ralph.json"),
        code = RalphcConfig.write(RalphcConfig.defaultParsedConfig),
        config = RalphcConfig.defaultParsedConfig
      )

    // build the std dependency
    val dependencyBuild =
      Dependency.compile(
        parsed = parsed,
        currentBuild = None
      ).asInstanceOf[BuildState.BuildCompiled]

    // dependency should exists in the build
    dependencyBuild.dependency shouldBe defined

    // return the build (the build contains the std workspace)
    dependencyBuild
  }

}
