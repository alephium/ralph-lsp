package org.alephium.ralph.lsp.pc.sourcecode.imports

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.error.ImportError
import org.alephium.ralph.lsp.access.compiler.message.SourceIndex
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, TestSourceCode}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.EitherValues._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.net.URI
import scala.collection.immutable.ArraySeq

class ImporterSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  "typeCheck" should {
    "return empty" when {
      "there are no import statements" in {
        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

        // expect no file IO because the code is in memory
        implicit val file: FileAccess =
          null

        val parsed =
          TestSourceCode
            .genParsed(
              """
                |Contract MyContract(id:U256){
                |  pub fn getId() -> U256 {
                |    return id
                |  }
                |}
                |""".stripMargin
            )
            .sample
            .get

        val importedCode =
          Importer.typeCheck(
            sourceCode = ArraySeq(parsed),
            dependency = None
          )

        // there are no imports
        importedCode.value shouldBe empty
      }
    }

    "return imported code" when {
      "there is a single import" in {
        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

        // expect no file IO because the code is in memory
        implicit val file: FileAccess =
          null

        // create and compile a dependency file
        val dependency =
          TestSourceCode
            .genCompiled(
              fileURI =
                URI.create("my_package/my_file.ral"), // the import statement here would be `import "my_package/my_file"`
              code =
                """
                  |Contract ImportedContract(id: U256) {
                  |  pub fn getId() -> U256 {
                  |    return id
                  |  }
                  |}
                  |""".stripMargin,
            )
            .map(_.asInstanceOf[SourceCodeState.Compiled]) // No errors. Successfully compiled.
            .sample
            .get

        // parse myCode with the above dependant code imported
        val myCode =
          TestSourceCode
            .genParsed(
              s"""
                 |// import the above code
                 |import "my_package/my_file"
                 |
                 |Contract MyContract(imported: ImportedContract) {
                 |  pub fn callIt() -> U256 {
                 |    return imported.getId()
                 |  }
                 |}
                 |""".stripMargin
            )
            .sample
            .get

        // type-check myCode and expect the dependency to be returned.
        val importedCode =
          Importer.typeCheck(
            sourceCode = ArraySeq(myCode),
            dependency = Some(ArraySeq(dependency))
          ).value

        // type check returns the dependency.
        importedCode should contain only dependency
      }
    }

    "report unknown imports" when {
      "there is a single import statement" in {
        implicit val compiler: CompilerAccess =
          CompilerAccess.ralphc

        // expect no file IO because the code is in memory
        implicit val file: FileAccess =
          null

        val myCode =
          TestSourceCode
            .genParsed(
              s"""
                 |// this import does not exist
                 |import "my_package/my_file"
                 |
                 |Contract MyContract(imported: ImportedContract) {
                 |  pub fn callIt() -> U256 {
                 |    return imported.getId()
                 |  }
                 |}
                 |""".stripMargin
            )
            .sample
            .get

        // type-check myCode and expect error to be returned because the import does not exists.
        val actualError =
          Importer.typeCheck(
            sourceCode = ArraySeq(myCode),
            dependency = None
          ).left.value

        // The error must report the import's AST as UnknownImport
        val expectedAST = {
          val my_packageName = "my_package/my_file"
          val my_packageNameQuoted = s""""$my_packageName""""
          val fullImport = s"""import $my_packageNameQuoted"""

          Tree.Import(
            pkg =
              Tree.StringLiteral(
                value = my_packageName,
                index =
                  SourceIndex(
                    index = myCode.code.lastIndexOf(my_packageNameQuoted),
                    // + 2 because the parser fetches the tail end spaces
                    width = my_packageNameQuoted.length + 2
                  )
              ),
            index =
              SourceIndex(
                index = myCode.code.lastIndexOf(fullImport),
                // + 2 because the parser fetches the tail end spaces
                width = fullImport.length + 2
              )
          )
        }

        val expectedImportError =
          ImportError.Unknown(expectedAST)

        val expectedError =
          SourceCodeState.ErrorSource(
            fileURI = myCode.fileURI,
            code = myCode.code,
            previous = Some(myCode),
            errors = ArraySeq(expectedImportError),
          )

        actualError should contain only expectedError
      }
    }
  }
}
