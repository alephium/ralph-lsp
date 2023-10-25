package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.pc.sourcecode.imports._
import org.alephium.ralph.lsp.GenCommon._
import org.alephium.ralph.lsp.access.file.FileAccess
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class SourceCodeSpec extends AnyWordSpec with Matchers with MockFactory with ScalaCheckDrivenPropertyChecks {

  "parse" should {
    "handle std imports" in {
      forAll(genFileURI(), Gen.someOf(StdInterface.stdInterfaces)) {
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
}
