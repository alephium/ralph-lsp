package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.GenCommon._
import org.alephium.ralph.lsp.pc.sourcecode.{GenSourceCode, SourceCodeState}
import org.alephium.ralph.lsp.pc.workspace.build.BuildState.BuildCompiled
import org.alephium.ralph.lsp.pc.workspace.build.WorkspaceBuild
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

  def genBuildCompiled(): Gen[BuildCompiled] =
    for {
      workspacePath <- genFolder()
      ralphcConfig <- genRalphcConfig(workspacePath.toUri)
    } yield
      BuildCompiled(
        buildURI = workspacePath.resolve(WorkspaceBuild.BUILD_FILE_NAME).toUri,
        code = WorkspaceBuild.writeConfig(ralphcConfig),
        config = ralphcConfig
      )

  def genInitialised(): Gen[WorkspaceState.Created] =
    for {
      workspaceURI <- genFolder()
    } yield WorkspaceState.Created(workspaceURI.toUri)

  def genUnCompiled(sourceCode: Gen[List[SourceCodeState]] = Gen.listOf(GenSourceCode.genSourceCode())): Gen[WorkspaceState.UnCompiled] =
    for {
      build <- genBuildCompiled()
      sourceCode <- sourceCode
    } yield
      WorkspaceState.UnCompiled(
        build = build,
        sourceCode = sourceCode.to(ArraySeq)
      )

  def genParsed(sourceCode: Gen[List[SourceCodeState.Parsed]] = Gen.listOf(GenSourceCode.genParsed())): Gen[WorkspaceState.Parsed] =
    for {
      config <- genBuildCompiled()
      sourceCode <- sourceCode
    } yield
      WorkspaceState.Parsed(
        build = config,
        sourceCode = sourceCode.to(ArraySeq)
      )

  def genErrored(sourceCode: Gen[List[SourceCodeState.Parsed]] = Gen.listOf(GenSourceCode.genParsed())): Gen[WorkspaceState.Errored] =
    for {
      sourceCode <- sourceCode
      errors <- Gen.sequence(sourceCode.map(parsed => genErrors(parsed.code)))
      parsed <- GenWorkspace.genParsed()
    } yield
      WorkspaceState.Errored(
        sourceCode = sourceCode.to(ArraySeq),
        workspaceErrors = errors.asScala.flatten.to(ArraySeq),
        parsed = parsed
      )

  def genCompiled(sourceCode: Gen[List[SourceCodeState.Parsed]] = Gen.listOf(GenSourceCode.genParsed())): Gen[WorkspaceState.Compiled] =
    for {
      sourceCode <- sourceCode
      parsed <- GenWorkspace.genParsed()
    } yield
      WorkspaceState.Compiled(
        sourceCode = sourceCode.to(ArraySeq),
        parsed = parsed
      )

  def genWorkspace(): Gen[WorkspaceState] =
    Gen.oneOf(
      genInitialised(),
      genUnCompiled(),
      genParsed(),
      genErrored(),
      genCompiled(),
    )

  def genAtLeastOneFailed(): Gen[WorkspaceState.UnCompiled] =
    genUnCompiled(GenSourceCode.genAtLeastOneFailed())

  def genParsedOrCompiledWithAtLeastOneFailed(): Gen[WorkspaceState.UnCompiled] =
    genUnCompiled(GenSourceCode.genParsedOrCompiledWithAtLeastOneFailed())

}
