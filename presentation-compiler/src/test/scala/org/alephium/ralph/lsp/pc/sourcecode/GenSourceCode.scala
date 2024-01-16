package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.FileIO
import org.alephium.ralph.lsp.GenCommon._
import org.alephium.ralph.lsp.pc.workspace.build.{BuildState, GenBuild}
import org.scalacheck.Gen

import java.net.URI
import java.nio.file.Paths

/**
 * Implements generators for [[SourceCode]] & [[SourceCodeState]] related data-types.
 */
object GenSourceCode {

  def genOnDisk(fileURI: Gen[URI] = genFileURI()): Gen[SourceCodeState.OnDisk] =
    fileURI map SourceCodeState.OnDisk

  /** */
  def genOnDiskForRoot(rootURI: Gen[URI] = genFolderURI()): Gen[SourceCodeState.OnDisk] =
    for {
      rootURI <- rootURI
      workspacePath = Gen.const(Paths.get(rootURI))
      fileURI = genFileURI(rootFolder = workspacePath)
      sourceCode <- GenSourceCode.genOnDisk(fileURI)
    } yield
      sourceCode

  /** Generate random source files within the build's [[BuildState.BuildParsed.config.contractPath]] */
  def genOnDiskForBuild(build: Gen[BuildState.BuildParsed] = GenBuild.genBuildParsed()): Gen[SourceCodeState.OnDisk] =
    for {
      build <- build
      workspacePath = Gen.const(Paths.get(build.workspaceURI.resolve(build.config.contractPath)))
      fileURI = genFileURI(rootFolder = workspacePath)
      sourceCode <- GenSourceCode.genOnDisk(fileURI)
    } yield
      sourceCode

  def genUnCompiled(code: Gen[String] = genGoodCode(),
                    fileURI: Gen[URI] = genFileURI()): Gen[SourceCodeState.UnCompiled] =
    for {
      code <- code
      fileURI <- fileURI
    } yield
      SourceCodeState.UnCompiled(
        fileURI = fileURI,
        code = code
      )

  def genInitialised(code: Gen[String] = genGoodCode(),
                     fileURI: Gen[URI] = genFileURI()): Gen[SourceCodeState.IsInitialised] =
    Gen.oneOf(
      genOnDisk(fileURI),
      genUnCompiled(code, fileURI)
    )

  def persist[S <: SourceCodeState](sourceCode: S,
                                    code: Gen[String] = genGoodCode()): S =
    sourceCode match {
      case aware: SourceCodeState.IsCodeAware =>
        FileIO.write(aware.fileURI, aware.code)
        sourceCode

      case aware: SourceCodeState =>
        FileIO.write(aware.fileURI, code.sample.get)
        sourceCode
    }

  def persistAll[I <: Iterable[SourceCodeState]](sourceCode: I,
                                                 code: Gen[String] = genGoodCode()): I = {
    sourceCode foreach (persist(_, code))
    sourceCode
  }

  def delete(sourceCode: SourceCodeState): Unit =
    FileIO.delete(sourceCode.fileURI)

  def deleteAll(sourceCode: Iterable[SourceCodeState]): Unit =
    sourceCode foreach delete
}
