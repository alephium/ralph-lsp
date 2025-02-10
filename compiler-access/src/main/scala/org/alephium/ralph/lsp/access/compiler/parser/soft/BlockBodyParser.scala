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
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object BlockBodyParser {

  /**
   * Parses a block's body such that a virtual block (curly braces), i.e. `{}` is defined.
   *
   * This function is invoked only for the root parser call.
   *
   * To parse a block's body when a parent block is already defined, for example,
   * within a contract or a function, use [[parseOrFailChild]].
   *
   * @return Parsed content of a block.
   */
  def parseOrFailRoot[Unknown: P]: P[SoftAST.BlockBody] =
    parseOrFail(
      isRootBlock = true,
      stop = Seq.empty
    )

  /**
   * Parses a block's body such a parent block is already defined.
   * For example, within a parent contract or a function.
   *
   * @return Parsed content of a block.
   */
  def parseOrFailChild[Unknown: P](stop: Token*): P[SoftAST.BlockBody] =
    parseOrFail(
      isRootBlock = false,
      stop = stop
    )

  private def parseOrFail[Unknown: P](
      isRootBlock: Boolean,
      stop: Seq[Token]): P[SoftAST.BlockBody] =
    P {
      Index ~
        SpaceParser.parseOrFail.? ~
        bodyPart(isRootBlock, stop).rep ~
        Index
    } map {
      case (from, headSpace, parts, to) =>
        SoftAST.BlockBody(
          index = range(from, to),
          prePartsSpace = headSpace,
          parts = parts
        )
    }

  private def bodyPart[Unknown: P](
      isRootBlock: Boolean,
      stop: Seq[Token]): P[SoftAST.BlockBodyPart] =
    P {
      Index ~
        part(isRootBlock, stop) ~
        SpaceParser.parseOrFail.? ~
        Index
    } map {
      case (from, ast, space, to) =>
        SoftAST.BlockBodyPart(
          index = range(from, to),
          part = ast,
          postPartSpace = space
        )
    }

  private def part[Unknown: P](
      isRootBlock: Boolean,
      stop: Seq[Token]): P[SoftAST.BodyPartAST] =
    P {
      TemplateParser.parseOrFail |
        EventParser.parseOrFail |
        StructParser.parseOrFail |
        FunctionParser.parseOrFail |
        ImportParser.parseOrFail |
        InheritanceParser.parseOrFail |
        expression(isRootBlock) |
        CommentParser.parseOrFail |
        UnresolvedParser.parseOrFail(stop: _*)
    }

  private def expression[Unknown: P](isRootBlock: Boolean): P[SoftAST.BodyPartAST] =
    if (isRootBlock)
      ExpressionBlockParser.parseOrFail
    else
      ExpressionParser.parseOrFail

}
