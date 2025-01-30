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
          importToken = Import(indexOf(">>import<<")),
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
          importToken = Import(indexOf(">>import<< ")),
          postImportSpace = Some(SpaceOne(indexOf("import>> <<"))),
          string = None
        )
    }

    "starting quote is defined without space" in {
      val importAST =
        parseImport("import\"")

      importAST shouldBe
        SoftAST.Import(
          index = indexOf(">>import\"<<"),
          importToken = Import(indexOf(">>import<<")),
          postImportSpace = None,
          string = Some(
            SoftAST.StringLiteral(
              index = indexOf("import>>\"<<"),
              startQuote = Quote(indexOf("import>>\"<<")),
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
          importToken = Import(indexOf(""">>import<< "folder/file.ral"""")),
          postImportSpace = Some(SpaceOne(indexOf("""import>> <<"folder/file.ral""""))),
          string = Some(
            SoftAST.StringLiteral(
              index = indexOf("""import >>"folder/file.ral"<<"""),
              startQuote = Quote(indexOf("""import >>"<<folder/file.ral"""")),
              head = Some(SoftAST.CodeString(indexOf("""import ">>folder<</file.ral""""), "folder")),
              tail = Seq(
                SoftAST.Path(
                  index = indexOf("""import "folder>>/file.ral<<""""),
                  slash = ForwardSlash(indexOf("""import "folder>>/<<file.ral"""")),
                  text = SoftAST.CodeString(indexOf("""import "folder/>>file.ral<<""""), "file.ral")
                )
              ),
              endQuote = Quote(indexOf("""import "folder/file.ral>>"<<"""))
            )
          )
        )
    }
  }

}
