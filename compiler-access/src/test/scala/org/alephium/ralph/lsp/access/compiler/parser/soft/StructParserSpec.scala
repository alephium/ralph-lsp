// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.OptionValues._

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
          structToken = Struct(">>struct<<"),
          preIdentifierSpace = None,
          identifier = IdentifierExpected("struct>><<"),
          preParamSpace = None,
          params = SoftAST.Group(
            index = indexOf("struct>><<"),
            openToken = Some(SoftAST.TokenExpected(indexOf("struct>><<"), Token.OpenCurly)),
            preHeadExpressionSpace = None,
            headExpression = None,
            postHeadExpressionSpace = None,
            tailExpressions = Seq.empty,
            closeToken = Some(SoftAST.TokenExpected(indexOf("struct>><<"), Token.CloseCurly))
          )
        )
    }

    "closing curly is missing" in {
      val struct = parseStruct("struct MyStruct{varName: TypeName")

      struct.structToken shouldBe Struct(">>struct<< MyStruct{varName: TypeName")
      struct.identifier shouldBe Identifier("struct >>MyStruct<<{varName: TypeName")

      // Tuples are tested in TupleSpec, test for the index and string code here.
      struct.params.index shouldBe indexOf("struct MyStruct>>{varName: TypeName<<")
      struct.params.toCode() shouldBe "{varName: TypeName"
      struct.params.closeToken.value shouldBe SoftAST.TokenExpected(indexOf("struct MyStruct{varName: TypeName>><<"), Token.CloseCurly)
    }

    "well defined struct" in {
      val struct = parseStruct("struct Bar { z: U256, mut foo: Foo }")

      struct.structToken shouldBe Struct(">>struct<< Bar { z: U256, mut foo: Foo }")
      struct.identifier shouldBe Identifier("struct >>Bar<< { z: U256, mut foo: Foo }")

      // Tuples are tested in TupleSpec, test for the index and string code here.
      struct.params.index shouldBe indexOf("struct Bar >>{ z: U256, mut foo: Foo }<<")
      struct.params.toCode() shouldBe "{ z: U256, mut foo: Foo }"
      struct.params.openToken.value shouldBe OpenCurly("struct Bar >>{<< z: U256, mut foo: Foo }")
      struct.params.closeToken.value shouldBe CloseCurly("struct Bar { z: U256, mut foo: Foo >>}<<")
    }

    "type is missing" when {
      "single type param" in {
        val struct = parseStruct("struct MyStruct{ name }")

        struct shouldBe
          SoftAST.Struct(
            index = indexOf(">>struct MyStruct{ name }<<"),
            structToken = Struct(">>struct<< MyStruct{ name }"),
            preIdentifierSpace = Some(Space("struct>> <<MyStruct{ name }")),
            identifier = Identifier("struct >>MyStruct<<{ name }"),
            preParamSpace = None,
            params = SoftAST.Group(
              index = indexOf("struct MyStruct>>{ name }<<"),
              openToken = Some(OpenCurly("struct MyStruct>>{<< name }")),
              preHeadExpressionSpace = Some(Space("struct MyStruct{>> <<name }")),
              headExpression = Some(Unresolved("struct MyStruct{ >>name<< }")),
              postHeadExpressionSpace = Some(Space("struct MyStruct{ name>> <<}")),
              tailExpressions = Seq.empty,
              closeToken = Some(CloseCurly("struct MyStruct{ name >>}<<"))
            )
          )
      }

      "3 type params" in {
        // `one` and `three` are stored as unresolved
        // `two` is stored with its right-hand-side stored as expression-expected
        val struct = parseStruct("struct MyStruct{ one, two:, three }")

        struct shouldBe
          SoftAST.Struct(
            index = indexOf(">>struct MyStruct{ one, two:, three }<<"),
            structToken = Struct(">>struct<< MyStruct{ one, two:, three }"),
            preIdentifierSpace = Some(Space("struct>> <<MyStruct{ one, two:, three }")),
            identifier = Identifier("struct >>MyStruct<<{ one, two:, three }"),
            preParamSpace = None,
            params = SoftAST.Group(
              index = indexOf("struct MyStruct>>{ one, two:, three }<<"),
              openToken = Some(OpenCurly("struct MyStruct>>{<< one, two:, three }")),
              preHeadExpressionSpace = Some(Space("struct MyStruct{>> <<one, two:, three }")),
              headExpression = Some(Unresolved("struct MyStruct{ >>one<<, two:, three }")),
              postHeadExpressionSpace = None,
              tailExpressions = Seq(
                SoftAST.GroupTail(
                  index = indexOf("struct MyStruct{ one>>, two:<<, three }"),
                  comma = Comma("struct MyStruct{ one>>,<< two:, three }"),
                  preExpressionSpace = Some(Space("struct MyStruct{ one,>> <<two:, three }")),
                  expression = SoftAST.TypeAssignment(
                    index = indexOf("struct MyStruct{ one, >>two:<<, three }"),
                    annotations = Seq.empty,
                    expressionLeft = Identifier("struct MyStruct{ one, >>two<<:, three }"),
                    preColonSpace = None,
                    colon = Colon("struct MyStruct{ one, two>>:<<, three }"),
                    postColonSpace = None,
                    expressionRight = ExpressionExpected("struct MyStruct{ one, two:>><<, three }")
                  ),
                  postExpressionSpace = None
                ),
                SoftAST.GroupTail(
                  index = indexOf("struct MyStruct{ one, two:>>, three <<}"),
                  comma = Comma("struct MyStruct{ one, two:>>,<< three }"),
                  preExpressionSpace = Some(Space("struct MyStruct{ one, two:,>> <<three }")),
                  expression = Unresolved("struct MyStruct{ one, two:, >>three<< }"),
                  postExpressionSpace = Some(Space("struct MyStruct{ one, two:, three>> <<}"))
                )
              ),
              closeToken = Some(CloseCurly("struct MyStruct{ one, two:, three >>}<<"))
            )
          )
      }

    }

  }

}
