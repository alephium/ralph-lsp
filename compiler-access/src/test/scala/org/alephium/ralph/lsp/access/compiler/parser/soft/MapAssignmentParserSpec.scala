// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MapAssignmentParserSpec extends AnyWordSpec with Matchers {

  "mapping types and identifier are not provided" in {
    val ast =
      parseMapAssignment("mapping")

    ast shouldBe
      SoftAST.MapAssignment(
        index = indexOf(">>mapping<<"),
        mapping = Mapping(">>mapping<<"),
        preTypesSpace = None,
        types = SoftAST.TypeParamsExpected(indexOf("mapping>><<")),
        preIdentifierSpace = None,
        identifier = IdentifierExpected("mapping>><<")
      )
  }

  "open bracket is provided" in {
    val ast =
      parseMapAssignment("mapping[")

    ast shouldBe
      SoftAST.MapAssignment(
        index = indexOf(">>mapping[<<"),
        mapping = Mapping(">>mapping<<["),
        preTypesSpace = None,
        types = SoftAST.ArrayInline(
          SoftAST.Group(
            index = indexOf("mapping>>[<<"),
            openToken = Some(OpenBracket("mapping>>[<<")),
            preHeadExpressionSpace = None,
            headExpression = None,
            postHeadExpressionSpace = None,
            tailExpressions = Seq.empty,
            closeToken = Some(TokenExpected("mapping[>><<", Token.BlockBracket))
          )
        ),
        preIdentifierSpace = None,
        identifier = IdentifierExpected("mapping[>><<")
      )
  }

  "key type is provided" in {
    val ast =
      parseMapAssignment("mapping[Key")

    ast shouldBe
      SoftAST.MapAssignment(
        index = indexOf(">>mapping[Key<<"),
        mapping = Mapping(">>mapping<<[Key"),
        preTypesSpace = None,
        types = SoftAST.ArrayInline(
          SoftAST.Group(
            index = indexOf("mapping>>[Key<<"),
            openToken = Some(OpenBracket("mapping>>[<<Key")),
            preHeadExpressionSpace = None,
            headExpression = Some(Identifier("mapping[>>Key<<")),
            postHeadExpressionSpace = None,
            tailExpressions = Seq.empty,
            closeToken = Some(TokenExpected("mapping[Key>><<", Token.BlockBracket))
          )
        ),
        preIdentifierSpace = None,
        identifier = IdentifierExpected("mapping[Key>><<")
      )
  }

  "identifier is missing" in {
    val ast =
      parseMapAssignment("mapping[Key, Value]")

    ast.types.toCode() shouldBe "[Key, Value]"
    ast.identifier shouldBe IdentifierExpected("mapping[Key, Value]>><<")
  }

}
