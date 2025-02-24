// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http:www.gnu.org/licenses/.
package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class EnumParserSpec extends AnyWordSpec {

  "fail" when {
    "enum is followed by a non-boundary character" in {
      val enumAST = parseSoft("enumMyEnum")

      enumAST.parts should have size 1
      enumAST.parts.head shouldBe Identifier(">>enumMyEnum<<")
    }
  }

  "pass" when {
    "only `enum` is defined" in {
      val enumAST = parseEnum("enum")

      enumAST shouldBe
        SoftAST.Enum(
          index = indexOf(">>enum<<"),
          enumToken = Enum(">>enum<<"),
          preIdentifierSpace = None,
          identifier = SoftAST.IdentifierExpected(indexOf("enum>><<")),
          preBlockSpace = None,
          block = None
        )
    }

    "`enum`, block braces and empty values block are defined" in {
      val enumAST = parseEnum("enum {}")

      enumAST shouldBe
        SoftAST.Enum(
          index = indexOf(">>enum {}<<"),
          enumToken = Enum(">>enum<<"),
          preIdentifierSpace = Some(Space("enum>> <<{}")),
          identifier = SoftAST.IdentifierExpected(indexOf("enum >><<{}")),
          preBlockSpace = None,
          block = Some(
            SoftAST.Block(
              index = indexOf("enum >>{}<<"),
              openCurly = OpenCurly("enum >>{<<}"),
              parts = Seq.empty,
              closeCurly = CloseCurly("enum {>>}<<")
            )
          )
        )
    }

    "`enum`, block braces and empty values block with a single space are defined" in {
      val enumAST = parseEnum("enum { }")

      enumAST shouldBe
        SoftAST.Enum(
          index = indexOf(">>enum { }<<"),
          enumToken = Enum(">>enum<< { }"),
          preIdentifierSpace = Some(Space("enum>> <<{ }")),
          identifier = SoftAST.IdentifierExpected(indexOf("enum >><<{ }")),
          preBlockSpace = None,
          block = Some(
            SoftAST.Block(
              index = indexOf("enum >>{ }<<"),
              openCurly = OpenCurly("enum >>{<< }"),
              parts = Seq(Space("enum {>> <<}")),
              closeCurly = CloseCurly("enum { >>}<<")
            )
          )
        )
    }

    "an identifier is provided as enum value" in {
      // TODO: See https://github.com/alephium/ralph-lsp/issues/387
      //       The first expression should be marked as Unresolved instead of an Identifier.
      val enumAST = parseEnum("enum { blah }")

      enumAST shouldBe
        SoftAST.Enum(
          index = indexOf(">>enum { blah }<<"),
          enumToken = Enum(">>enum<< { blah }"),
          preIdentifierSpace = Some(Space("enum>> <<{ blah }")),
          identifier = SoftAST.IdentifierExpected(indexOf("enum >><<{ blah }")),
          preBlockSpace = None,
          block = Some(
            SoftAST.Block(
              index = indexOf("enum >>{ blah }<<"),
              openCurly = OpenCurly("enum >>{<< blah }"),
              parts = Seq(
                Space("enum {>> <<blah }"),
                Identifier("enum { >>blah<< }"),
                Space("enum { blah>> <<}")
              ),
              closeCurly = CloseCurly("enum { blah >>}<<")
            )
          )
        )
    }

    "a valid enum value is provided" in {
      val enumAST = parseEnum("enum { value = 1 }")

      enumAST shouldBe
        SoftAST.Enum(
          index = indexOf(">>enum { value = 1 }<<"),
          enumToken = Enum(">>enum<< { value = 1 }"),
          preIdentifierSpace = Some(Space("enum>> <<{ value = 1 }")),
          identifier = SoftAST.IdentifierExpected(indexOf("enum >><<{ value = 1 }")),
          preBlockSpace = None,
          block = Some(
            SoftAST.Block(
              index = indexOf("enum >>{ value = 1 }<<"),
              openCurly = OpenCurly("enum >>{<< value = 1 }"),
              Seq(
                Space("enum {>> <<value = 1 }"),
                SoftAST.Assignment(
                  index = indexOf("enum { >>value = 1<< }"),
                  expressionLeft = Identifier("enum { >>value<< = 1 }"),
                  postIdentifierSpace = Some(Space("enum { value>> <<= 1 }")),
                  equalToken = Equal("enum { value >>=<< 1 }"),
                  postEqualSpace = Some(Space("enum { value =>> <<1 }")),
                  expressionRight = Number("enum { value = >>1<< }")
                ),
                Space("enum { value = 1>> <<}")
              ),
              closeCurly = CloseCurly("enum { value = 1 >>}<<")
            )
          )
        )
    }

    "two valid enum value is provided" in {
      val enumAST = parseEnum("enum { one = 1 two = 2 }")

      enumAST shouldBe
        SoftAST.Enum(
          index = indexOf(">>enum { one = 1 two = 2 }<<"),
          enumToken = Enum(">>enum<< { one = 1 two = 2 }"),
          preIdentifierSpace = Some(Space("enum>> <<{ one = 1 two = 2 }")),
          identifier = SoftAST.IdentifierExpected(indexOf("enum >><<{ one = 1 two = 2 }")),
          preBlockSpace = None,
          block = Some(
            SoftAST.Block(
              index = indexOf("enum >>{ one = 1 two = 2 }<<"),
              openCurly = OpenCurly("enum >>{<< one = 1 two = 2 }"),
              Seq(
                Space("enum {>> <<one = 1 two = 2 }"),
                SoftAST.Assignment(
                  index = indexOf("enum { >>one = 1<< two = 2}"),
                  expressionLeft = Identifier("enum { >>one<< = 1 two = 2 }"),
                  postIdentifierSpace = Some(Space("enum { one>> <<= 1 two = 2 }")),
                  equalToken = Equal("enum { one >>=<< 1 two = 2 }"),
                  postEqualSpace = Some(Space("enum { one =>> <<1 two = 2 }")),
                  expressionRight = Number("enum { one = >>1<< two = 2 }")
                ),
                Space("enum { one = 1>> <<two = 2}"),
                SoftAST.Assignment(
                  index = indexOf("enum { one = 1 >>two = 2<< }"),
                  expressionLeft = Identifier("enum { one = 1 >>two<< = 2 }"),
                  postIdentifierSpace = Some(Space("enum { one = 1 two>> <<= 2 }")),
                  equalToken = Equal("enum { one = 1 two >>=<< 2 }"),
                  postEqualSpace = Some(Space("enum { one = 1 two =>> <<2 }")),
                  expressionRight = Number("enum { one = 1 two = >>2<< }")
                ),
                Space("enum { one = 1 two = 2>> <<}")
              ),
              closeCurly = CloseCurly("enum { one = 1 two = 2 >>}<<")
            )
          )
        )
    }

    "first EnumValue is identifier and the second one is valid" in {
      // TODO: See https://github.com/alephium/ralph-lsp/issues/387
      //       The first expression should be marked as Unresolved instead of an Identifier.
      val enumAST = parseEnum("enum { blah two = 2 }")

      enumAST shouldBe
        SoftAST.Enum(
          index = indexOf(">>enum { blah two = 2 }<<"),
          enumToken = Enum(">>enum<< { blah two = 2 }"),
          preIdentifierSpace = Some(Space("enum>> <<{ blah two = 2 }")),
          identifier = SoftAST.IdentifierExpected(indexOf("enum >><<{ blah two = 2 }")),
          preBlockSpace = None,
          block = Some(
            SoftAST.Block(
              index = indexOf("enum >>{ blah two = 2 }<<"),
              openCurly = OpenCurly("enum >>{<< blah two = 2 }"),
              Seq(
                Space("enum {>> <<blah two = 2 }"),
                Identifier("enum { >>blah<< two = 2 }"),
                Space("enum { blah>> <<two = 2 }"),
                SoftAST.Assignment(
                  index = indexOf("enum { blah >>two = 2<< }"),
                  expressionLeft = Identifier("enum { blah >>two<< = 2 }"),
                  postIdentifierSpace = Some(Space("enum { blah two>> <<= 2 }")),
                  equalToken = Equal("enum { blah two >>=<< 2 }"),
                  postEqualSpace = Some(Space("enum { blah two =>> <<2 }")),
                  expressionRight = Number("enum { blah two = >>2<< }")
                ),
                Space("enum { blah two = 2>> <<}")
              ),
              closeCurly = CloseCurly("enum { blah two = 2 >>}<<")
            )
          )
        )
    }
  }

}
