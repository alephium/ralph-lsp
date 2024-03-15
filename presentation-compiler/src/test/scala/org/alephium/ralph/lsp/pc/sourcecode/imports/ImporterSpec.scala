package org.alephium.ralph.lsp.pc.sourcecode.imports

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.TestFile
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.error.ImportError
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, TestSourceCode}
import org.scalatest.EitherValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

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

        val fileURI =
          TestFile
            .genFolderPath(true)
            .sample
            .get
            // the import statement for this file would be `import "my_package/my_file"`
            .resolve("my_package")
            .resolve("my_file.ral")
            .toUri

        // create and compile a dependency file
        val dependency =
          TestSourceCode
            .genCompiled(
              fileURI = fileURI,
              code = """
                  |Contract ImportedContract(id: U256) {
                  |  pub fn getId() -> U256 {
                  |    return id
                  |  }
                  |}
                  |""".stripMargin
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
          Importer
            .typeCheck(
              sourceCode = ArraySeq(myCode),
              dependency = Some(ArraySeq(dependency))
            )
            .value

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
          Importer
            .typeCheck(
              sourceCode = ArraySeq(myCode),
              dependency = None
            )
            .left
            .value

        // The error must report the import's AST as UnknownImport
        val expectedAST = {
          val my_package = "my_package"
          val my_file = "my_file"
          val my_package_file = s"$my_package/$my_file"
          val my_package_file_quoted = s""""$my_package_file""""
          val import_statement = s"""import $my_package_file_quoted"""
          val fileURI = Some(myCode.fileURI)

          Tree.Import(
            string = Tree.StringLiteral(
              value = my_package_file_quoted,
              name = Tree.Name(
                value = my_package_file,
                index = SourceIndex(
                  index = myCode.code.lastIndexOf(my_package_file),
                  width = my_package_file.length,
                  fileURI = fileURI
                )
              ),
              index = SourceIndex(
                index = myCode.code.lastIndexOf(my_package_file_quoted),
                width = my_package_file_quoted.length,
                fileURI = fileURI
              )
            ),
            path = Some(
              Tree.ImportPath(
                folder =
                  Tree.Name(my_package,
                            SourceIndex(myCode.code.lastIndexOf(my_package), my_package.length, fileURI = fileURI)
                           ),
                file =
                  Tree.Name(my_file, SourceIndex(myCode.code.lastIndexOf(my_file), my_file.length, fileURI = fileURI)),
                index = SourceIndex(myCode.code.lastIndexOf(my_package_file), my_package_file.length, fileURI = fileURI)
              )
            ),
            index = SourceIndex(
              index = myCode.code.lastIndexOf(import_statement),
              width = import_statement.length,
              fileURI = fileURI
            )
          )
        }

        val expectedImportError =
          ImportError.Unknown(expectedAST)

        val expectedError =
          SourceCodeState.ErrorCompilation(
            fileURI = myCode.fileURI,
            code = myCode.code,
            parsed = myCode,
            errors = ArraySeq(expectedImportError)
          )

        actualError should contain only expectedError
      }
    }
  }
}
