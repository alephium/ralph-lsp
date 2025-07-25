// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

object AssetApprovalParser {

  def parseOrFail[Unknown: P]: P[SoftAST.AssetApproval] =
    P {
      GroupParser.parseOrFail(
        assertNonEmpty = true,
        open = Token.OpenCurly,
        close = Token.CloseCurly,
        delimiter = Token.Semicolon,
        expressionsParseOrFail = assetGroup
      )
    } map SoftAST.AssetApproval

  private def assetGroup[Unknown: P] =
    P {
      GroupParser.parseOrFail(
        delimiter = Token.Comma,
        expressionsParseOrFail = expression
      )
    }

  private def expression[Unknown: P]: P[SoftAST.ExpressionAST] =
    P {
      ArrowAssignmentParser.parseOrFail |
        AssetAssignmentParser.parseOrFail |
        NumberParser.parseOrFail
    }

}
