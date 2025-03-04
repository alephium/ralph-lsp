package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.alephium.ralph.lsp.access.util.TestFastParse.assertIsFastParseError
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ImportParserSpec extends AnyWordSpec with Matchers {

  "fail" when {
    "import is a prefix of an identifier" in {
      assertIsFastParseError {
        parseImport("important")
      }
    }
  }

  "succeed" when {
    "only the import token is defined" in {
      val importAST =
        parseImport("import")

      importAST shouldBe
        SoftAST.Import(
          index = indexOf(">>import<<"),
          importToken = Import(">>import<<"),
          postImportSpace = None,
          string = None
        )
    }

    "tail space is defined" in {
      val importAST =
        parseImport("import ")

      importAST shouldBe
        SoftAST.Import(
          index = indexOf(">>import <<"),
          importToken = Import(">>import<< "),
          postImportSpace = Some(Space("import>> <<")),
          string = None
        )
    }

    "starting quote is defined without space" in {
      val importAST =
        parseImport("import\"")

      importAST shouldBe
        SoftAST.Import(
          index = indexOf(">>import\"<<"),
          importToken = Import(">>import<<"),
          postImportSpace = None,
          string = Some(
            SoftAST.StringLiteral(
              index = indexOf("import>>\"<<"),
              startQuote = Quote("import>>\"<<"),
              head = None,
              tail = Seq.empty,
              endQuote = SoftAST.TokenExpected(indexOf("import\">><<"), Token.Quote)
            )
          )
        )
    }

    "import is fully defined" in {
      val importAST =
        parseImport("""import "folder/file.ral"""")

      importAST shouldBe
        SoftAST.Import(
          index = indexOf(""">>import "folder/file.ral"<<"""),
          importToken = Import(""">>import<< "folder/file.ral""""),
          postImportSpace = Some(Space("""import>> <<"folder/file.ral"""")),
          string = Some(
            SoftAST.StringLiteral(
              index = indexOf("""import >>"folder/file.ral"<<"""),
              startQuote = Quote("""import >>"<<folder/file.ral""""),
              head = Some(SoftAST.CodeString(indexOf("""import ">>folder<</file.ral""""), "folder")),
              tail = Seq(
                SoftAST.Path(
                  index = indexOf("""import "folder>>/file.ral<<""""),
                  slash = ForwardSlash("""import "folder>>/<<file.ral""""),
                  text = SoftAST.CodeString(indexOf("""import "folder/>>file.ral<<""""), "file.ral")
                )
              ),
              endQuote = Quote("""import "folder/file.ral>>"<<""")
            )
          )
        )
    }
  }

  "stop" when {
    "the head path is the `import` keyword" in {
      val root =
        parseSoft("import \"import")

      // both `import` statements are parsed as individual `import` ASTs
      root.parts should have size 2

      root shouldBe
        SoftAST.RootBlock(
          index = indexOf(">>import \"import<<"),
          parts = Seq(
            SoftAST.Import(
              index = indexOf(">>import \"<<import"),
              importToken = Import(">>import<< \"import"),
              postImportSpace = Some(Space("import>> <<\"import")),
              string = Some(
                SoftAST.StringLiteral(
                  index = indexOf("import >>\"<<import"),
                  startQuote = Quote("import >>\"<<import"),
                  head = None,
                  tail = Seq.empty,
                  endQuote = TokenExpected("import \">><<import", Token.Quote)
                )
              )
            ),
            SoftAST.Import(
              index = indexOf("import \">>import<<"),
              importToken = Import("import \">>import<<"),
              postImportSpace = None,
              string = None
            )
          )
        )
    }

    "the second path is the `import` keyword" in {
      val root =
        parseSoft("import \"nft/import")

      // both `import` statements are parsed as individual `import` ASTs
      root.parts should have size 2

      root shouldBe
        SoftAST.RootBlock(
          index = indexOf(">>import \"nft/import<<"),
          parts = Seq(
            SoftAST.Import(
              index = indexOf(">>import \"nft/<<import"),
              importToken = Import(">>import<< \"nft/import"),
              postImportSpace = Some(Space("import>> <<\"nft/import")),
              string = Some(
                SoftAST.StringLiteral(
                  index = indexOf("import >>\"nft/<<import"),
                  startQuote = Quote("import >>\"<<nft/import"),
                  head = Some(CodeString("import \">>nft<</import")),
                  tail = Seq(
                    SoftAST.Path(
                      index = indexOf("import \"nft>>/<<import"),
                      slash = ForwardSlash("import \"nft>>/<<import"),
                      text = CodeStringExpected("import \"nft/>><<import")
                    )
                  ),
                  endQuote = TokenExpected("import \"nft/>><<import", Token.Quote)
                )
              )
            ),
            SoftAST.Import(
              index = indexOf("import \"nft/>>import<<"),
              importToken = Import("import \"nft/>>import<<"),
              postImportSpace = None,
              string = None
            )
          )
        )
    }
  }

}
