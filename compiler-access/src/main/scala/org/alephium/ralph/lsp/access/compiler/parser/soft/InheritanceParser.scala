package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse._
import fastparse.NoWhitespace.noWhitespaceImplicit
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.range
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}

private object InheritanceParser {

  /** Syntax:
   * {{{
   *   extends ContractOne(), Contract2(arg1, arg2) implements AnInterface
   * }}}
   * */
  def parseOrFail[Unknown: P]: P[SoftAST.Inheritance] =
    P {
      Index ~
        (TokenParser.parseOrFail(Token.Implements) | TokenParser.parseOrFail(Token.Extends)) ~
        TokenParser.isBoundary() ~
        SpaceParser.parseOrFail.? ~
        referenceCallOrIdentifier ~
        tailReferencesOrSpace ~
        Index
    } map {
      case (from, token, postInheritanceTypeSpace, headReference, tailReferencesOrSpace, to) =>
        SoftAST.Inheritance(
          index = range(from, to),
          inheritanceType = token,
          postInheritanceTypeSpace = postInheritanceTypeSpace,
          headReference = headReference,
          tailReferencesOrSpace = tailReferencesOrSpace
        )
    }

  /**
   * Tail references are those that follow the head reference.
   *
   * In the following example, `extends` has `A` as its head reference, and `B` and `C` are tail references:
   * {{{
   *   extends A, >>B(), C(param1, param2)<<
   * }}}
   *
   * If `extends` has no tail references, there may be an optional space after `A`,
   * in which case this function returns `Some(Left(space))`.
   * {{{
   *    extends A>> <<implements B
   * }}}
   *
   * If there are no space or references after `A` (for cases where a block `{}` follows)
   * this function will return [[None]].
   * {{{
   *   extends A{}
   * }}}
   */
  private def tailReferencesOrSpace[Unknown: P]: P[Option[Either[SoftAST.Space, Seq[SoftAST.TailReferences]]]] =
    P(tailReferences.rep) flatMap {
      references =>
        if (references.nonEmpty)
          Pass(Some(Right(references))) // references exists
        else
          SpaceParser.parseOrFail.?.map(_.map(Left(_))) // check if space exists, else return None
    }

  /** Comma separated references */
  private def tailReferences[Unknown: P]: P[SoftAST.TailReferences] =
    P {
      Index ~
        TokenParser.parseOrFail(Token.Comma) ~
        SpaceParser.parseOrFail.? ~
        referenceCallOrIdentifier ~
        SpaceParser.parseOrFail.? ~
        Index
    } map {
      case (from, comma, postCommaSpace, reference, postRefSpace, to) =>
        SoftAST.TailReferences(
          index = range(from, to),
          comma = comma,
          postCommaSpace = postCommaSpace,
          reference = reference,
          postReferenceSpace = postRefSpace
        )
    }

  private def referenceCallOrIdentifier[Unknown: P]: P[SoftAST.ReferenceCallOrIdentifier] =
    P(ReferenceCallParser.parseOrFail | IdentifierParser.parse)

}
