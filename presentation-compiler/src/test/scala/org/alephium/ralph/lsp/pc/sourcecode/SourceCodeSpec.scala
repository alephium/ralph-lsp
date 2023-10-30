package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.sourcecode.imports._
import org.alephium.ralph.lsp.pc.workspace.build.BuildDependencies
import org.alephium.ralph.lsp.GenCommon._
import org.alephium.ralph.lsp.access.file.FileAccess
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.alephium.ralph.lsp.pc.workspace.build.Build
import scala.collection.immutable.ArraySeq

class SourceCodeSpec extends AnyWordSpec with Matchers with MockFactory with ScalaCheckDrivenPropertyChecks {

  "parse" should {
    "handle std imports" in {
      forAll(genFileURI(), Gen.someOf(StdInterface.buildStdInterfaces.right.get)) {
        case (fileUri, interfaces) =>
          implicit val compiler: CompilerAccess = CompilerAccess.ralphc
          implicit val file: FileAccess = FileAccess.disk

          val code = interfaces.map { case (interface, _) =>
            s"""import "$interface""""
          }.mkString("", "\n", "\n") ++ "Contract Test(id:U256){}"

          val sourceState = SourceCodeState.UnCompiled(fileUri, code)
          val parsed = SourceCode.parse(sourceState)

          parsed shouldBe a[SourceCodeState.Parsed]

          parsed.asInstanceOf[SourceCodeState.Parsed].imports.map(_.name.value).toSet shouldBe interfaces.map(_._1).toSet
      }
    }
  }

  "compile" should {
    "support multiple time the same import in different file" in {
      val interfaces = StdInterface.buildStdInterfaces.right.get
      val buildDependencies = BuildDependencies(interfaces)
      val compilerOptions=CompilerOptions.Default
      implicit val compiler: CompilerAccess = CompilerAccess.ralphc
      implicit val file: FileAccess = FileAccess.disk

      forAll(Gen.nonEmptyListOf(genFileURI())) {
        case fileUris =>
          val multipleSameInterfaces = interfaces.keys ++ interfaces.keys
          val sourceStates = fileUris.zipWithIndex.map { case (fileUri, index) =>
            val code = multipleSameInterfaces.map { interface =>
              s"""import "$interface""""
            }.mkString("", "\n", "\n") ++ s"Contract Test$index(id:U256){pub fn test()->(){}}"
            SourceCodeState.UnCompiled(fileUri, code)
          }.map(SourceCode.parse(_).asInstanceOf[SourceCodeState.Parsed])


          val (error, compiled) = SourceCode.compile(ArraySeq.from(sourceStates), compilerOptions,buildDependencies)

          error shouldBe None
      }
    }
  }
}
