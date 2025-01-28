package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class StructParserSpec extends AnyWordSpec {

  "closing curly is missing" in {
    val struct = parseStruct("struct MyStruct{varName: TypeName")

    struct.structToken shouldBe Struct(indexOf(">>struct<< MyStruct{varName: TypeName"))
    struct.identifier shouldBe Identifier(indexOf("struct >>MyStruct<<{varName: TypeName"), "MyStruct")

    // Tuples are tested in TupleSpec, test for the index and string code here.
    struct.params.index shouldBe indexOf("struct MyStruct>>{varName: TypeName<<")
    struct.params.toCode() shouldBe "{varName: TypeName"
    struct.params.closeToken shouldBe SoftAST.TokenExpected(indexOf("struct MyStruct{varName: TypeName>><<"), Token.CloseCurly)
  }

  "well defined struct" in {
    val struct = parseStruct("struct Bar { z: U256, mut foo: Foo }")

    struct.structToken shouldBe Struct(indexOf(">>struct<< Bar { z: U256, mut foo: Foo }"))
    struct.identifier shouldBe Identifier(indexOf("struct >>Bar<< { z: U256, mut foo: Foo }"), "Bar")

    // Tuples are tested in TupleSpec, test for the index and string code here.
    struct.params.index shouldBe indexOf("struct Bar >>{ z: U256, mut foo: Foo }<<")
    struct.params.toCode() shouldBe "{ z: U256, mut foo: Foo }"
    struct.params.openToken shouldBe OpenCurly(indexOf("struct Bar >>{<< z: U256, mut foo: Foo }"))
    struct.params.closeToken shouldBe CloseCurly(indexOf("struct Bar { z: U256, mut foo: Foo >>}<<"))

  }

}
