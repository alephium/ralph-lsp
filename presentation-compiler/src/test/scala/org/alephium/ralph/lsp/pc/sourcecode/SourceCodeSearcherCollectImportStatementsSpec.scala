package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.ArraySeq

class SourceCodeSearcherCollectImportStatementsSpec extends AnyWordSpec with Matchers {

  implicit val file: FileAccess = FileAccess.disk
  implicit val compiler: CompilerAccess = CompilerAccess.ralphc

  "return empty" when {
    "input is empty" in {
      SourceCodeSearcher.collectImportStatements(ArraySeq.empty) shouldBe empty
    }
  }

  "collect distinct import statements" in {
    val goodCodeParsed =
      TestSourceCode.genParsed(
        """
          |import "blah/blah"
          |import "blah/blah"
          |import "std/fungible_token_interface"
          |import "std/fungible_token_interface"
          |
          |Contract MyContract() {
          |  fn function1() -> () {}
          |}
          |""".stripMargin
      ).sample.get.asInstanceOf[SourceCodeState.Parsed]

    val goodCodeCompiled =
      TestSourceCode.genCompiled(
        """
          |import "std/nft_collection_interface"
          |import "std/nft_collection_interface"
          |import "std/fungible_token_interface"
          |import "std/fungible_token_unimplemented"
          |
          |Abstract Contract AbstractContract() { }
          |
          |Contract MyContract() extends AbstractContract() {
          |  fn function1() -> () {}
          |}
          |""".stripMargin
      ).sample.get.asInstanceOf[SourceCodeState.Compiled].parsed

    val errorCompilation =
      TestSourceCode
        .genCompiled(
          """
            |import "std/nft_collection_with_royalty_interface"
            |import "std/nft_collection_with_royalty_interface"
            |import "std/nft_collection_interface"
            |import "std/nft_collection_interface"
            |
            |Contract MyContract() extends DoesNotExist() {
            |  fn function1() -> () {}
            |}
            |""".stripMargin
        ).sample.get.asInstanceOf[SourceCodeState.ErrorCompilation].parsed

    val allCode =
      ArraySeq(goodCodeParsed, goodCodeCompiled, errorCompilation)

    val actual =
      SourceCodeSearcher
        .collectImportStatements(allCode)
        .map(_.string.value)

    val expected =
      Array(
        "\"blah/blah\"",
        "\"std/fungible_token_interface\"",
        "\"std/fungible_token_unimplemented\"",
        "\"std/nft_collection_interface\"",
        "\"std/nft_collection_with_royalty_interface\""
      )

    actual should contain theSameElementsAs expected

    TestSourceCode deleteAllIfExists allCode
  }

}
