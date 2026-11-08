// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST

private object ReferenceCallParser {

  def parse[Unknown: P]: P[SoftAST.ReferenceCall] =
    parse(required = true)

  def parseOrFail[Unknown: P]: P[SoftAST.ReferenceCall] =
    parse(required = false)

  /**
   * Parses a function or object call along with its arguments [[GroupParser]].
   *
   * Syntax: reference(arg1, arg2, (arg3, arg4))
   *
   * @param required Determines if the parser should fail
   *                 when the reference name or the opening parenthesis is missing.
   * @return A parsed instance of [[SoftAST.ReferenceCall]].
   */
  private def parse[Unknown: P](required: Boolean): P[SoftAST.ReferenceCall] =
    P {
      Index ~
        IdentifierParser.parse(required) ~
        SpaceParser.parseOrFail.? ~
        AssetApprovalParser.parseOrFail.? ~
        SpaceParser.parseOrFail.?
    } flatMap {
      case (from, identifier, preAssetApprovalSpace, assetApproval, preArgumentsSpace) =>
        // An arrow distinguishes asset approval from a struct constructor. Once it is
        // present, keep the approval AST even while the call's argument tuple is missing.
        val hasApprovalArrow =
          assetApproval.exists(_.toNode.walk.exists(_.data.isInstanceOf[SoftAST.ArrowAssignment]))

        P {
          TupleParser.parse(required || hasApprovalArrow) ~ Index
        } map {
          case (arguments, to) =>
            SoftAST.ReferenceCall(
              index = range(from, to),
              reference = identifier,
              preAssetApprovalSpace = preAssetApprovalSpace,
              assetApproval = assetApproval,
              preArgumentsSpace = preArgumentsSpace,
              arguments = arguments
            )
        }
    }

}
