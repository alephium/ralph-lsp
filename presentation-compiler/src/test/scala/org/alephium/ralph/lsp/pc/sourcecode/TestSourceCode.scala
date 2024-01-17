package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.{TestCode, TestFile}
import org.alephium.ralph.lsp.TestFile._
import org.alephium.ralph.lsp.pc.util.URIUtil
import org.alephium.ralph.lsp.pc.workspace.build.{BuildState, TestBuild}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers._

import java.net.URI
import java.nio.file.Paths

/** [[SourceCode]] related test functions */
object TestSourceCode {

  def genOnDisk(fileURI: Gen[URI] = genFileURI()): Gen[SourceCodeState.OnDisk] =
    fileURI map SourceCodeState.OnDisk

  /** */
  def genOnDiskForRoot(rootURI: Gen[URI] = genFolderURI()): Gen[SourceCodeState.OnDisk] =
    for {
      rootURI <- rootURI
      workspacePath = Gen.const(Paths.get(rootURI))
      fileURI = genFileURI(rootFolder = workspacePath)
      sourceCode <- TestSourceCode.genOnDisk(fileURI)
    } yield {
      // assert that SourceCode URI is a child of rootURI
      URIUtil.contains(rootURI, sourceCode.fileURI) shouldBe true
      sourceCode
    }

  /** Generate random source files within the build's [[BuildState.BuildParsed.config.contractPath]] */
  def genOnDiskForBuild(build: Gen[BuildState.BuildParsed] = TestBuild.genBuildParsed()): Gen[SourceCodeState.OnDisk] =
    for {
      build <- build
      workspacePath = Gen.const(Paths.get(build.workspaceURI.resolve(build.config.contractPath)))
      fileURI = genFileURI(rootFolder = workspacePath)
      sourceCode <- TestSourceCode.genOnDisk(fileURI)
    } yield
      sourceCode

  def genUnCompiled(code: Gen[String] = TestCode.genGoodCode(),
                    fileURI: Gen[URI] = genFileURI()): Gen[SourceCodeState.UnCompiled] =
    for {
      code <- code
      fileURI <- fileURI
    } yield
      SourceCodeState.UnCompiled(
        fileURI = fileURI,
        code = code
      )

  def genInitialised(code: Gen[String] = TestCode.genGoodCode(),
                     fileURI: Gen[URI] = genFileURI()): Gen[SourceCodeState.IsInitialised] =
    Gen.oneOf(
      genOnDisk(fileURI),
      genUnCompiled(code, fileURI)
    )

  def persist[S <: SourceCodeState](sourceCode: S,
                                    code: Gen[String] = TestCode.genGoodCode()): S =
    sourceCode match {
      case aware: SourceCodeState.IsCodeAware =>
        TestFile.write(aware.fileURI, aware.code)
        sourceCode

      case aware: SourceCodeState =>
        TestFile.write(aware.fileURI, code.sample.get)
        sourceCode
    }

  def persistAll[I <: Iterable[SourceCodeState]](sourceCode: I,
                                                 code: Gen[String] = TestCode.genGoodCode()): I = {
    sourceCode foreach (persist(_, code))
    sourceCode
  }

  def delete(sourceCode: SourceCodeState): Unit =
    TestFile.delete(sourceCode.fileURI)

  def deleteAll(sourceCode: Iterable[SourceCodeState]): Unit =
    sourceCode foreach delete
}
