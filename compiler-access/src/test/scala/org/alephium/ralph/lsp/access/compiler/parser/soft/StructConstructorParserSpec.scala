// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.alephium.ralph.lsp.access.util.TestFastParse.assertIsFastParseError
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class StructConstructorParserSpec extends AnyWordSpec {

  "fail" when {
    "identifier only" in {
      // Identifier on its own is not a constructor call
      assertIsFastParseError {
        parseStructConstructorNoCodeCheck("MyStruct")
      }
    }

    "identifier followed by space" in {
      // Identifier on its own is not a constructor call
      assertIsFastParseError {
        parseStructConstructorNoCodeCheck("MyStruct ")
      }
    }

    "block only" in {
      // A block on its own is not a constructor call
      assertIsFastParseError {
        parseStructConstructorNoCodeCheck("{}")
      }
    }
  }

  "identifier followed by open curly" should {
    "parse as a constructor" in {
      val ast = parseStructConstructor("MyStruct {")

      ast shouldBe
        SoftAST.StructConstructor(
          index = indexOf(">>MyStruct {<<"),
          identifier = Identifier(">>MyStruct<< {"),
          preParamSpace = Some(Space("MyStruct>> <<{")),
          params = SoftAST.Group(
            index = indexOf("MyStruct >>{<<"),
            openToken = Some(OpenCurly("MyStruct >>{<<")),
            preHeadExpressionSpace = None,
            headExpression = None,
            postHeadExpressionSpace = None,
            tailExpressions = Seq.empty,
            closeToken = Some(TokenExpected("MyStruct {>><<", Token.CloseCurly))
          )
        )
    }
  }

  "identifier followed by an empty block" should {
    "parse as a constructor" in {
      val ast = parseStructConstructor("MyStruct {}")

      ast shouldBe
        SoftAST.StructConstructor(
          index = indexOf(">>MyStruct {}<<"),
          identifier = Identifier(">>MyStruct<< {}"),
          preParamSpace = Some(Space("MyStruct>> <<{}")),
          params = SoftAST.Group(
            index = indexOf("MyStruct >>{}<<"),
            openToken = Some(OpenCurly("MyStruct >>{<<}")),
            preHeadExpressionSpace = None,
            headExpression = None,
            postHeadExpressionSpace = None,
            tailExpressions = Seq.empty,
            closeToken = Some(CloseCurly("MyStruct {>>}<<"))
          )
        )
    }
  }

  "field with complex type params" should {
    "parse as a constructor" in {
      val ast = parseStructConstructor("MyStruct {array: [1, 2, 3], string: b`String`}")

      // Skip asserting params
      ast.copy(params = null) shouldBe
        SoftAST.StructConstructor(
          index = indexOf(">>MyStruct {array: [1, 2, 3], string: b`String`}<<"),
          identifier = Identifier(">>MyStruct<< {array: [1, 2, 3], string: b`String`}"),
          preParamSpace = Some(Space("MyStruct>> <<{array: [1, 2, 3], string: b`String`}")),
          params = null
        )

      // Assert params
      val expressions = ast.params.expressions
      expressions should have size 2

      expressions.head shouldBe a[SoftAST.StructFieldAssignment]
      expressions.head.toCode() shouldBe "array: [1, 2, 3]"

      expressions.last shouldBe a[SoftAST.StructFieldAssignment]
      expressions.last.toCode() shouldBe "string: b`String`"
    }
  }

  "identifier followed by a nonempty block" should {
    "parse as a constructor" in {
      val ast = parseStructConstructor("MyStruct {number: 1}")

      ast shouldBe
        SoftAST.StructConstructor(
          index = indexOf(">>MyStruct {number: 1}<<"),
          identifier = Identifier(">>MyStruct<< {number: 1}"),
          preParamSpace = Some(Space("MyStruct>> <<{number: 1}")),
          params = SoftAST.Group(
            index = indexOf("MyStruct >>{number: 1}<<"),
            openToken = Some(OpenCurly("MyStruct >>{<<number: 1}")),
            preHeadExpressionSpace = None,
            headExpression = Some(
              SoftAST.StructFieldAssignment(
                index = indexOf("MyStruct {>>number: 1<<}"),
                expressionLeft = Identifier("MyStruct {>>number<<: 1}"),
                preColonSpace = None,
                colon = Colon("MyStruct {number>>:<< 1}"),
                preExpressionSpace = Some(Space("MyStruct {number:>> <<1}")),
                expressionRight = Number("MyStruct {number: >>1<<}")
              )
            ),
            postHeadExpressionSpace = None,
            tailExpressions = Seq.empty,
            closeToken = Some(CloseCurly("MyStruct {number: 1>>}<<"))
          )
        )
    }
  }

  "value is missing" when {
    "single value param" in {
      val ast = parseStructConstructor("MyStruct{ name }")

      ast shouldBe
        SoftAST.StructConstructor(
          index = indexOf(">>MyStruct{ name }<<"),
          identifier = Identifier(">>MyStruct<<{ name }"),
          preParamSpace = None,
          params = SoftAST.Group(
            index = indexOf("MyStruct>>{ name }<<"),
            openToken = Some(OpenCurly("MyStruct>>{<< name }")),
            preHeadExpressionSpace = Some(Space("MyStruct{>> <<name }")),
            headExpression = Some(
              SoftAST.StructFieldAssignment(
                index = indexOf("MyStruct{ >>name <<}"),
                expressionLeft = Identifier("MyStruct{ >>name<< }"),
                preColonSpace = Some(Space("MyStruct{ name>> <<}")),
                colon = TokenExpected("MyStruct{ name >><<}", Token.Colon),
                preExpressionSpace = None,
                expressionRight = ExpressionExpected("MyStruct{ name >><<}")
              )
            ),
            postHeadExpressionSpace = None,
            tailExpressions = Seq.empty,
            closeToken = Some(CloseCurly("MyStruct{ name >>}<<"))
          )
        )
    }

    "3 value params" in {
      // `one` and `three` are stored as unresolved
      // `two` is stored with its right-hand-side stored as expression-expected
      val ast = parseStructConstructor("MyStruct{ one, two:, three }")

      ast shouldBe
        SoftAST.StructConstructor(
          index = indexOf(">>MyStruct{ one, two:, three }<<"),
          identifier = Identifier(">>MyStruct<<{ one, two:, three }"),
          preParamSpace = None,
          params = SoftAST.Group(
            index = indexOf("MyStruct>>{ one, two:, three }<<"),
            openToken = Some(OpenCurly("MyStruct>>{<< one, two:, three }")),
            preHeadExpressionSpace = Some(Space("MyStruct{>> <<one, two:, three }")),
            headExpression = Some(
              SoftAST.StructFieldAssignment(
                index = indexOf("MyStruct{ >>one<<, two:, three }"),
                expressionLeft = Identifier("MyStruct{ >>one<<, two:, three }"),
                preColonSpace = None,
                colon = TokenExpected("MyStruct{ one>><<, two:, three }", Token.Colon),
                preExpressionSpace = None,
                expressionRight = ExpressionExpected("MyStruct{ one>><<, two:, three }")
              )
            ),
            postHeadExpressionSpace = None,
            tailExpressions = Seq(
              SoftAST.GroupTail(
                index = indexOf("MyStruct{ one>>, two:<<, three }"),
                comma = Comma("MyStruct{ one>>,<< two:, three }"),
                preExpressionSpace = Some(Space("MyStruct{ one,>> <<two:, three }")),
                expression = SoftAST.StructFieldAssignment(
                  index = indexOf("MyStruct{ one, >>two:<<, three }"),
                  expressionLeft = Identifier("MyStruct{ one, >>two<<:, three }"),
                  preColonSpace = None,
                  colon = Colon("MyStruct{ one, two>>:<<, three }"),
                  preExpressionSpace = None,
                  expressionRight = ExpressionExpected("MyStruct{ one, two:>><<, three }")
                ),
                postExpressionSpace = None
              ),
              SoftAST.GroupTail(
                index = indexOf("MyStruct{ one, two:>>, three <<}"),
                comma = Comma("MyStruct{ one, two:>>,<< three }"),
                preExpressionSpace = Some(Space("MyStruct{ one, two:,>> <<three }")),
                expression = SoftAST.StructFieldAssignment(
                  index = indexOf("MyStruct{ one, two:, >>three <<}"),
                  expressionLeft = Identifier("MyStruct{ one, two:, >>three<< }"),
                  preColonSpace = Some(Space("MyStruct{ one, two:, three>> <<}")),
                  colon = TokenExpected("MyStruct{ one, two:, three >><<}", Token.Colon),
                  preExpressionSpace = None,
                  expressionRight = ExpressionExpected("MyStruct{ one, two:, three >><<}")
                ),
                postExpressionSpace = None
              )
            ),
            closeToken = Some(CloseCurly("MyStruct{ one, two:, three >>}<<"))
          )
        )
    }
  }

  "unresolved" when {
    "invalid single token parameter" in {
      val ast = parseStructConstructor("MyStruct{ ⚠️ }")

      ast shouldBe
        SoftAST.StructConstructor(
          index = indexOf(">>MyStruct{ ⚠️ }<<"),
          identifier = Identifier(">>MyStruct<<{ ⚠️ }"),
          preParamSpace = None,
          params = SoftAST.Group(
            index = indexOf("MyStruct>>{ ⚠️ }<<"),
            openToken = Some(OpenCurly("MyStruct>>{<< ⚠️ }")),
            preHeadExpressionSpace = Some(Space("MyStruct{>> <<⚠️ }")),
            headExpression = Some(Unresolved("MyStruct{ >>⚠️<< }")),
            postHeadExpressionSpace = Some(Space("MyStruct{ ⚠️>> <<}")),
            tailExpressions = Seq.empty,
            closeToken = Some(CloseCurly("MyStruct{ ⚠️ >>}<<"))
          )
        )
    }

    "invalid single double parameter" in {
      val ast = parseStructConstructor("MyStruct{ ⚠️⚠️ }")

      ast shouldBe
        SoftAST.StructConstructor(
          index = indexOf(">>MyStruct{ ⚠️⚠️ }<<"),
          identifier = Identifier(">>MyStruct<<{ ⚠️⚠️ }"),
          preParamSpace = None,
          params = SoftAST.Group(
            index = indexOf("MyStruct>>{ ⚠️⚠️ }<<"),
            openToken = Some(OpenCurly("MyStruct>>{<< ⚠️⚠️ }")),
            preHeadExpressionSpace = Some(Space("MyStruct{>> <<⚠️⚠️ }")),
            headExpression = Some(Unresolved("MyStruct{ >>⚠️⚠️<< }")),
            postHeadExpressionSpace = Some(Space("MyStruct{ ⚠️⚠️>> <<}")),
            tailExpressions = Seq.empty,
            closeToken = Some(CloseCurly("MyStruct{ ⚠️⚠️ >>}<<"))
          )
        )
    }
  }

}
