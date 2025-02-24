package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class StructParserSpec extends AnyWordSpec {

  "fail" when {
    "struct is followed by a non-boundary character" in {
      val struct = parseSoft("structMyStruct")

      struct.parts should have size 1
      struct.parts.head shouldBe
        Identifier(
          index = indexOf(">>structMyStruct<<"),
          text = "structMyStruct"
        )
    }
  }

  "pass" when {
    "struct is defined" in {
      val struct = parseStruct("struct")

      struct shouldBe
        SoftAST.Struct(
          index = indexOf(">>struct<<"),
          structToken = Struct(indexOf(">>struct<<")),
          preIdentifierSpace = None,
          identifier = SoftAST.IdentifierExpected(indexOf("struct>><<")),
          preParamSpace = None,
          params = SoftAST.Group(
            index = indexOf("struct>><<"),
            openToken = SoftAST.TokenExpected(indexOf("struct>><<"), Token.OpenCurly),
            preHeadExpressionSpace = None,
            headExpression = None,
            postHeadExpressionSpace = None,
            tailExpressions = Seq.empty,
            closeToken = SoftAST.TokenExpected(indexOf("struct>><<"), Token.CloseCurly)
          )
        )
    }

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

}
