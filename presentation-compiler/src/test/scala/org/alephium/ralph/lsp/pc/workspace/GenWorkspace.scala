package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.pc.config.GenCommon._
import org.alephium.ralph.lsp.pc.sourcecode.GenSourceCode
import org.alephium.ralphc.Config
import org.scalacheck.{Arbitrary, Gen}

import java.net.URI
import java.nio.file.Paths
import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters.CollectionHasAsScala

object GenWorkspace {

  def genCompilerOptions(): Gen[CompilerOptions] =
    for {
      ignoreUnusedConstantsWarnings <- Arbitrary.arbitrary[Boolean]
      ignoreUnusedVariablesWarnings <- Arbitrary.arbitrary[Boolean]
      ignoreUnusedFieldsWarnings <- Arbitrary.arbitrary[Boolean]
      ignoreUnusedPrivateFunctionsWarnings <- Arbitrary.arbitrary[Boolean]
      ignoreUpdateFieldsCheckWarnings <- Arbitrary.arbitrary[Boolean]
      ignoreCheckExternalCallerWarnings <- Arbitrary.arbitrary[Boolean]
    } yield
      CompilerOptions(
        ignoreUnusedConstantsWarnings = ignoreUnusedConstantsWarnings,
        ignoreUnusedVariablesWarnings = ignoreUnusedVariablesWarnings,
        ignoreUnusedFieldsWarnings = ignoreUnusedFieldsWarnings,
        ignoreUnusedPrivateFunctionsWarnings = ignoreUnusedPrivateFunctionsWarnings,
        ignoreUpdateFieldsCheckWarnings = ignoreUpdateFieldsCheckWarnings,
        ignoreCheckExternalCallerWarnings = ignoreCheckExternalCallerWarnings
      )

  def genRalphcConfig(workspaceURI: URI): Gen[Config] = {
    val workspacePath = Paths.get(workspaceURI)
    for {
      compilerOptions <- genCompilerOptions()
      contractPath <- genFolder().map(workspacePath.resolve)
      artifactPath <- genFolder().map(workspacePath.resolve)
    } yield
      Config(
        compilerOptions = compilerOptions,
        contractPath = contractPath,
        artifactPath = artifactPath
      )
  }

  def genWorkspaceConfig(): Gen[WorkspaceConfig] =
    for {
      workspaceURI <- genFolder()
      ralphcConfig <- genRalphcConfig(workspaceURI.toUri)
    } yield
      WorkspaceConfig(
        workspaceURI = workspaceURI.toUri,
        ralphcConfig = ralphcConfig
      )

  def genUnConfigured(): Gen[WorkspaceState.UnConfigured] =
    for {
      workspaceURI <- genFolder()
    } yield WorkspaceState.UnConfigured(workspaceURI.toUri)

  def genUnCompiled(): Gen[WorkspaceState.UnCompiled] =
    for {
      config <- genWorkspaceConfig()
      sourceCodes <- Gen.listOf(GenSourceCode.genSourceCode())
    } yield
      WorkspaceState.UnCompiled(
        config = config,
        sourceCodeStates = sourceCodes.to(ArraySeq)
      )

  def genParsed(): Gen[WorkspaceState.Parsed] =
    for {
      config <- genWorkspaceConfig()
      sourceCodes <- Gen.listOf(GenSourceCode.genParsed())
    } yield
      WorkspaceState.Parsed(
        config = config,
        sourceCodeStates = sourceCodes.to(ArraySeq)
      )

  def genCompiled(): Gen[WorkspaceState.Compiled] =
    for {
      sourceCodes <- Gen.listOf(GenSourceCode.genParsed())
      errors <- Gen.sequence(sourceCodes.map(parsed => genErrors(parsed.code)))
      parsed <- GenWorkspace.genParsed()
    } yield
      WorkspaceState.Compiled(
        sourceCodeStates = sourceCodes.to(ArraySeq),
        workspaceErrors = errors.asScala.flatten.to(ArraySeq),
        previousState = parsed
      )

  def genWorkspace(): Gen[WorkspaceState] =
    Gen.oneOf(
      genUnConfigured(),
      genUnCompiled(),
      genParsed(),
      genCompiled()
    )

}
