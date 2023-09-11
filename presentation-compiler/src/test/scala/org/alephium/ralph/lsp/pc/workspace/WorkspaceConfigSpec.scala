package org.alephium.ralph.lsp.pc.workspace

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.TryValues._

class WorkspaceConfigSpec extends AnyWordSpec with Matchers {

  "read config" should {
    "parse workspace config" in {
      val config =
        """
          |{
          |  "compilerOptions": {
          |    "ignoreUnusedConstantsWarnings": false,
          |    "ignoreUnusedVariablesWarnings": false,
          |    "ignoreUnusedFieldsWarnings": false,
          |    "ignoreUnusedPrivateFunctionsWarnings": false,
          |    "ignoreUpdateFieldsCheckWarnings": false,
          |    "ignoreCheckExternalCallerWarnings": false
          |  },
          |  "contractPath": "./contracts",
          |  "artifactPath": "./artifacts"
          |}
          |""".stripMargin

      val expected = WorkspaceConfig.defaultRalphcConfig
      val actual = WorkspaceConfig.readConfig(config).success.value

      actual shouldBe expected
    }
  }
}
