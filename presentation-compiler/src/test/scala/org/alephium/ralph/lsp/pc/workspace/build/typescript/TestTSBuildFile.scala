// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace.build.typescript

import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary

object TestTSBuildFile {

  def genCompilerOptions: Gen[TSConfig.CompilerOptions] =
    for {
      ignoreUnusedConstantsWarnings        <- Gen.option(arbitrary[Boolean])
      ignoreUnusedVariablesWarnings        <- Gen.option(arbitrary[Boolean])
      ignoreUnusedFieldsWarnings           <- Gen.option(arbitrary[Boolean])
      ignoreUnusedPrivateFunctionsWarnings <- Gen.option(arbitrary[Boolean])
      ignoreUpdateFieldsCheckWarnings      <- Gen.option(arbitrary[Boolean])
      ignoreCheckExternalCallerWarnings    <- Gen.option(arbitrary[Boolean])
      ignoreUnusedFunctionReturnWarnings   <- Gen.option(arbitrary[Boolean])
      errorOnWarnings                      <- Gen.option(arbitrary[Boolean])
    } yield TSConfig.CompilerOptions(
      ignoreUnusedConstantsWarnings,
      ignoreUnusedVariablesWarnings,
      ignoreUnusedFieldsWarnings,
      ignoreUnusedPrivateFunctionsWarnings,
      ignoreUpdateFieldsCheckWarnings,
      ignoreCheckExternalCallerWarnings,
      ignoreUnusedFunctionReturnWarnings,
      errorOnWarnings
    )

  def genTSConfig: Gen[TSConfig] =
    for {
      sourceDir       <- Gen.option(Gen.alphaStr)
      artifactDir     <- Gen.option(Gen.alphaStr)
      compilerOptions <- Gen.option(genCompilerOptions)
    } yield TSConfig(sourceDir, artifactDir, compilerOptions)

  def genTSBuildFile(tsConfig: TSConfig): Gen[String] =
    for {
      imports <- Gen.listOfN(5, genTSImport)
      sep     <- Gen.oneOf("'", "\"")
    } yield s"""|${imports.mkString("\n")}
                |
                |const configuration: Configuration<Settings> = {
                |${printOption("sourceDir", tsConfig.sourceDir, sep)}
                |${printOption("artifactDir", tsConfig.artifactDir, sep)}
                |${printCompilerOptions(tsConfig.compilerOptions)}
                |}
                |
                |export default configuration;
            """.stripMargin

  val genTSImport: Gen[String] =
    for {
      imports     <- Gen.listOfN(5, Gen.alphaChar)
      packageName <- Gen.alphaChar
    } yield s"import { ${imports.mkString(", ")} } from '$packageName'"

  private def printCompilerOptions(compilerOptionsOpt: Option[TSConfig.CompilerOptions]): String =
    compilerOptionsOpt.fold("") {
      compilerOptions =>
        s"""compilerOptions: {
        |  ${printOption("ignoreUnusedConstantsWarnings", compilerOptions.ignoreUnusedConstantsWarnings)}
        |  ${printOption("ignoreUnusedVariablesWarnings", compilerOptions.ignoreUnusedVariablesWarnings)}
        |  ${printOption("ignoreUnusedFieldsWarnings", compilerOptions.ignoreUnusedFieldsWarnings)}
        |  ${printOption("ignoreUnusedPrivateFunctionsWarnings", compilerOptions.ignoreUnusedPrivateFunctionsWarnings)}
        |  ${printOption("ignoreUpdateFieldsCheckWarnings", compilerOptions.ignoreUpdateFieldsCheckWarnings)}
        |  ${printOption("ignoreCheckExternalCallerWarnings", compilerOptions.ignoreCheckExternalCallerWarnings)}
        |  ${printOption("ignoreUnusedFunctionReturnWarnings", compilerOptions.ignoreUnusedFunctionReturnWarnings)}
        |  ${printOption("skipAbstractContractCheck", compilerOptions.skipAbstractContractCheck)}
        |  ${printOption("errorOnWarnings", compilerOptions.errorOnWarnings)}
        |}""".stripMargin
    }

  private def printOption[A](
      name: String,
      value: Option[A],
      surround: String = ""): String =
    value.fold("")(
      value => s"$name: $surround$value$surround,"
    )

}
