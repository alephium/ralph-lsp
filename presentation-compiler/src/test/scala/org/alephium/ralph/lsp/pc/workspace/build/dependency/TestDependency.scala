package org.alephium.ralph.lsp.pc.workspace.build.dependency

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.log.ClientLogger
import org.alephium.ralph.lsp.pc.workspace.build.{Build, BuildState, RalphcConfig}
import org.scalatest.matchers.should.Matchers._

import java.nio.file.Paths

object TestDependency {

  /** Build the standard library */
  def buildStd(): BuildState.BuildCompiled = {
    implicit val logger: ClientLogger =
      TestClientLogger

    implicit val file: FileAccess =
      null

    implicit val compiler: CompilerAccess =
      CompilerAccess.ralphc

    // create a default build file.
    val parsed =
      BuildState.BuildParsed(
        buildURI = Paths.get(Build.BUILD_FILE_NAME).toUri,
        code = RalphcConfig.write(RalphcConfig.defaultParsedConfig),
        config = RalphcConfig.defaultParsedConfig
      )

    // build the std dependency
    val dependencyBuild =
      Dependency
        .compile(
          parsed = parsed,
          currentBuild = None
        )
        .asInstanceOf[BuildState.BuildCompiled]

    // dependency should exists in the build
    dependencyBuild.dependency shouldBe defined

    // return the build (the build contains the std workspace)
    dependencyBuild
  }

}
