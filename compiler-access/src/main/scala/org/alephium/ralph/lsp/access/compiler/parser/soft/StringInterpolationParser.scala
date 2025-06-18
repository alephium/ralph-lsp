// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.utils.CollectionUtil

private object StringInterpolationParser {

  /** Parses an interpolated string. */
  def parseOrFail[Unknown: P]: P[SoftAST.StringInterpolation] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Tick) ~
        interpolate.rep ~
        CodeParser.parse(Token.Tick) ~
        Index
    } map {
      case (from, startTick, interpolation, endTick, to) =>
        SoftAST.StringInterpolation(
          index = range(from, to),
          startTick = startTick,
          interpolation = CollectionUtil.mergeConsecutive(interpolation),
          endTick = endTick
        )
    }

  private def interpolate[Unknown: P]: P[Either[Seq[SoftAST.Code], Seq[SoftAST.InterpolatedBlock]]] =
    P {
      // or without interpolation
      nonInterpolation.map(Left(_)) |
        // with interpolation
        interpolation.rep(1).map(Right(_))
    }

  /** Parse non-interpolated text */
  private def nonInterpolation[Unknown: P]: P[Seq[SoftAST.Code]] =
    P {
      // Parse text not containing the dollar
      TextParser
        .parseOrFail(Token.Dollar, Token.Tick)
        .map(Seq(_)) |
        // Parse escaped interpolation
        CodeParser
          .parseOrFail(Token.Dollar)
          .rep(exactly = 2)
    }

  /** Parse interpolated text */
  private def interpolation[Unknown: P]: P[SoftAST.InterpolatedBlock] =
    P {
      Index ~
        // Ensure that it's not an escaped interpolation. These should've already been processed by `nonInterpolation`.
        (CodeParser.parseOrFail(Token.Dollar) ~ !Token.Dollar.lexeme) ~
        // Interpolation syntax is valid. A block is required.
        BlockParser.parse ~
        Index
    } map {
      case (from, dollar, block, to) =>
        SoftAST.InterpolatedBlock(
          index = range(from, to),
          dollar = dollar,
          block = block
        )
    }

}
