package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.TestFile._
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.error.TestError
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.util.URIUtil
import org.alephium.ralph.lsp.pc.workspace.build.dependency.TestDependency
import org.alephium.ralph.lsp.pc.workspace.build.{BuildState, TestBuild}
import org.alephium.ralph.lsp.{TestCode, TestFile}
import org.scalacheck.Gen
import org.scalatest.EitherValues._
import org.scalatest.matchers.should.Matchers._

import java.net.URI
import java.nio.file.Paths
import scala.collection.immutable.ArraySeq

/** [[SourceCode]] related test functions */
object TestSourceCode {

  def genOnDisk(fileURI: Gen[URI] = genFileURI()): Gen[SourceCodeState.OnDisk] =
    fileURI map SourceCodeState.OnDisk.apply

  /** Generate a source code file on-disk */
  def genOnDiskAndPersist(
      fileURI: Gen[URI] = genFileURI(),
      code: Gen[String] = TestCode.genGoodCode()): Gen[(SourceCodeState.OnDisk, String)] =
    for {
      onDisk <- genOnDisk(fileURI)
      code   <- code
    } yield {
      // write code to the source file
      val persistedOnDisk = persist(onDisk, code)
      (persistedOnDisk, code)
    }

  /**
   */
  def genOnDiskForRoot(rootURI: Gen[URI] = genFolderURI()): Gen[SourceCodeState.OnDisk] =
    for {
      rootURI <- rootURI
      workspacePath = Gen.const(Paths.get(rootURI))
      fileURI       = genFileURI(rootFolder = workspacePath)
      sourceCode <- TestSourceCode.genOnDisk(fileURI)
    } yield {
      // assert that SourceCode URI is a child of rootURI
      URIUtil.contains(rootURI, sourceCode.fileURI) shouldBe true
      sourceCode
    }

  /** Generate random source files within the build's [[BuildState.BuildParsed.config.contractPath]] */
  def genOnDiskForBuild(build: Gen[BuildState.BuildParsed] = TestBuild.genParsed()): Gen[SourceCodeState.OnDisk] =
    for {
      build <- build
      workspacePath = Gen.const(Paths.get(build.workspaceURI.resolve(build.config.contractPath)))
      fileURI       = genFileURI(rootFolder = workspacePath)
      sourceCode <- TestSourceCode.genOnDisk(fileURI)
    } yield sourceCode

  /**
   * Generate an error state for a source-code state.
   * These error states
   */
  def genErrorState(
      state: Gen[SourceCodeState] = genOnDisk(),
      code: Gen[String] = TestCode.genGoodCode()): Gen[SourceCodeState.IsError] =
    Gen.oneOf(
      genErrorAccess(state),
      genParserError(state, code)
      // TODO: Add genCompilationError
    )

  /**
   * Generate an error state for a source-code state.
   * These error states
   */
  def genErrorAccess(state: Gen[SourceCodeState] = genOnDisk()): Gen[SourceCodeState.ErrorAccess] =
    for {
      _state <- state
      error  <- TestError.genError()
    } yield SourceCodeState.ErrorAccess(
      fileURI = _state.fileURI,
      error = error
    )

  def genParserError(
      state: Gen[SourceCodeState] = genOnDisk(),
      code: Gen[String] = TestCode.genGoodCode()): Gen[SourceCodeState.ErrorParser] =
    for {
      _state <- state
      _code  <- code
      errors <- Gen.listOf(TestError.genError())
    } yield SourceCodeState.ErrorParser(
      fileURI = _state.fileURI,
      code = _code,
      errors = errors
    )

  def genUnCompiled(
      code: Gen[String] = TestCode.genGoodCode(),
      fileURI: Gen[URI] = genFileURI()): Gen[SourceCodeState.UnCompiled] =
    for {
      code    <- code
      fileURI <- fileURI
    } yield SourceCodeState.UnCompiled(
      fileURI = fileURI,
      code = code
    )

  def genInitialised(
      code: Gen[String] = TestCode.genGoodCode(),
      fileURI: Gen[URI] = genFileURI()): Gen[SourceCodeState.IsInitialised] =
    Gen.oneOf(
      genOnDisk(fileURI),
      genUnCompiled(code, fileURI)
    )

  def genParsedOK(
      code: Gen[String] = TestCode.genGoodCode(),
      fileURI: Gen[URI] = genFileURI()
    )(implicit file: FileAccess,
      compiler: CompilerAccess): Gen[SourceCodeState.Parsed] =
    genParsed(
      code = code,
      fileURI = fileURI
    ).map(_.asInstanceOf[SourceCodeState.Parsed])

  def genParsed(
      code: Gen[String] = TestCode.genGoodCode(),
      fileURI: Gen[URI] = genFileURI()
    )(implicit file: FileAccess,
      compiler: CompilerAccess): Gen[SourceCodeState] = {
    val unCompiled =
      genUnCompiled(
        code = code,
        fileURI = fileURI
      )

    genParsed(unCompiled)
  }

  def genParsed(
      unCompiled: Gen[SourceCodeState.UnCompiled]
    )(implicit file: FileAccess,
      compiler: CompilerAccess): Gen[SourceCodeState] =
    unCompiled map SourceCode.parse

  def genCompiledOK(
      code: Gen[String] = TestCode.genGoodCode(),
      fileURI: Gen[URI] = genFileURI()
    )(implicit file: FileAccess,
      compiler: CompilerAccess): Gen[SourceCodeState.Compiled] =
    genCompiled(
      code = code,
      fileURI = fileURI
    ).map(_.asInstanceOf[SourceCodeState.Compiled])

  def genCompiled(
      code: Gen[String] = TestCode.genGoodCode(),
      fileURI: Gen[URI] = genFileURI()
    )(implicit file: FileAccess,
      compiler: CompilerAccess): Gen[SourceCodeState.IsParsed] = {
    val parsed =
      genParsedOK(
        code = code,
        fileURI = fileURI
      )

    genCompiled(parsed)
  }

  def genCompiled(parsed: Gen[SourceCodeState.Parsed])(implicit compiler: CompilerAccess): Gen[SourceCodeState.IsParsed] =
    parsed map compile

  def compile(parsed: SourceCodeState.Parsed)(implicit compiler: CompilerAccess): SourceCodeState.IsParsed = {
    val result =
      SourceCode.compile(
        sourceCode = ArraySeq(parsed),
        dependency = TestDependency.buildStd().dependencies.flatMap(_.sourceCode),
        compilerOptions = CompilerOptions.Default,
        workspaceErrorURI = parsed.fileURI
      )

    result.value should have size 1
    result.value.head
  }

  def persist[S <: SourceCodeState](
      sourceCode: S,
      code: Gen[String] = TestCode.genGoodCode()): S =
    sourceCode match {
      case aware: SourceCodeState.IsCodeAware =>
        TestFile.write(aware.fileURI, aware.code)
        sourceCode

      case aware: SourceCodeState =>
        TestFile.write(aware.fileURI, code.sample.get)
        sourceCode
    }

  def persistAll[I <: Iterable[SourceCodeState]](
      sourceCode: I,
      code: Gen[String] = TestCode.genGoodCode()): I = {
    sourceCode foreach (persist(_, code))
    sourceCode
  }

  def delete(sourceCode: SourceCodeState): Unit =
    TestFile.delete(sourceCode.fileURI)

  def deleteIfExists(sourceCode: SourceCodeState): Boolean =
    TestFile.deleteIfExists(sourceCode.fileURI)

  def deleteAll(sourceCode: Iterable[SourceCodeState]): Unit =
    sourceCode foreach delete

  def deleteAllIfExists(sourceCode: Iterable[SourceCodeState]): Unit =
    sourceCode foreach deleteIfExists

}
