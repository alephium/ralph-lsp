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

package org.alephium.ralph.lsp.access.compiler.parser.soft.ast

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST.collectASTs
import org.alephium.ralph.lsp.utils.Node

sealed trait SoftAST extends Product { self =>

  def index: SourceIndex

  final def children(): Iterator[SoftAST] =
    productIterator flatMap collectASTs

  final def toNode(): Node[this.type, SoftAST] =
    Node(
      data = self,
      children = children().map(_.toNode()).toSeq
    )

  final def toCode(): String =
    toNode().toCode()

  final def toStringTree(): String =
    toNode().toStringTree()

  def toStringPretty(): String =
    if (self.children().nonEmpty)
      s"${self.getClass.getSimpleName}: ${self.index}"
    else
      self.toString

}

object SoftAST {

  implicit class NodeSoftASTExtensions(val node: Node[SoftAST, SoftAST]) extends AnyVal {

    def toCode(): String =
      node.walkDown.foldLeft("") {
        case (code, Node(ast: CodeAST, _)) =>
          code + ast.code

        case (code, _) =>
          code
      }

    def toStringTree(): String =
      node.toStringTree(_.toStringPretty())

  }

  sealed trait BodyPartAST extends SoftAST

  sealed trait CodeAST extends SoftAST {

    def code: String

  }

  abstract class TokenAST(token: Token) extends CodeAST {

    final override def code: String =
      token.lexeme

  }

  abstract class ErrorAST(val message: String)       extends SoftAST
  abstract class ExpectedErrorAST(element: String)   extends ErrorAST(s"$element expected")
  abstract class TokenExpectedErrorAST(token: Token) extends ExpectedErrorAST(s"'${token.lexeme}'")

  case class Fn(index: SourceIndex)                 extends TokenAST(Token.Fn)
  case class Comma(index: SourceIndex)              extends TokenAST(Token.Comma)
  case class DoubleForwardSlash(index: SourceIndex) extends TokenAST(Token.DoubleForwardSlash)

  sealed abstract class TemplateToken(token: Token) extends TokenAST(token)
  case class Contract(index: SourceIndex)           extends TemplateToken(Token.Contract)
  case class TxScript(index: SourceIndex)           extends TemplateToken(Token.TxScript)

  sealed trait ColonAST                        extends SoftAST
  case class Colon(index: SourceIndex)         extends TokenAST(Token.Colon) with ColonAST
  case class ColonExpected(index: SourceIndex) extends TokenExpectedErrorAST(Token.Colon) with ColonAST

  sealed trait ForwardArrowAST                        extends SoftAST
  case class ForwardArrow(index: SourceIndex)         extends TokenAST(Token.ForwardArrow) with ForwardArrowAST
  case class ForwardArrowExpected(index: SourceIndex) extends TokenExpectedErrorAST(Token.ForwardArrow) with ForwardArrowAST

  sealed trait OpenParenAST                        extends SoftAST
  case class OpenParen(index: SourceIndex)         extends TokenAST(Token.OpenParen) with OpenParenAST
  case class OpenParenExpected(index: SourceIndex) extends TokenExpectedErrorAST(Token.OpenParen) with OpenParenAST

  sealed trait CloseParenAST                        extends SoftAST
  case class CloseParen(index: SourceIndex)         extends TokenAST(Token.CloseParen) with CloseParenAST
  case class CloseParenExpected(index: SourceIndex) extends TokenExpectedErrorAST(Token.CloseParen) with CloseParenAST

  sealed trait OpenCurlyAST                        extends SoftAST
  case class OpenCurly(index: SourceIndex)         extends TokenAST(Token.OpenCurly) with OpenCurlyAST
  case class OpenCurlyExpected(index: SourceIndex) extends TokenExpectedErrorAST(Token.OpenCurly) with OpenCurlyAST

  sealed trait CloseCurlyAST                        extends SoftAST
  case class CloseCurly(index: SourceIndex)         extends TokenAST(Token.CloseCurly) with CloseCurlyAST
  case class CloseCurlyExpected(index: SourceIndex) extends TokenExpectedErrorAST(Token.CloseCurly) with CloseCurlyAST

  case class Template(
      index: SourceIndex,
      templateType: TemplateToken,
      preIdentifierSpace: SpaceAST,
      identifier: SoftAST.IdentifierAST,
      preParamSpace: Option[Space],
      params: ParameterClauseAST,
      postParamSpace: Option[Space],
      block: BlockClause)
    extends BodyPartAST

  case class BlockClause(
      index: SourceIndex,
      openCurly: OpenCurlyAST,
      preBodySpace: Option[Space],
      body: BlockBody,
      postBodySpace: Option[Space],
      closeCurly: CloseCurlyAST)
    extends SoftAST

  case class BlockBody(
      index: SourceIndex,
      prePartsSpace: Option[Space],
      parts: Seq[BlockBodyPart])
    extends SoftAST

  case class BlockBodyPart(
      index: SourceIndex,
      part: BodyPartAST,
      postPartSpace: Option[Space])
    extends SoftAST

  sealed trait ParameterClauseAST extends SoftAST

  case class EmptyParameterClause(
      index: SourceIndex,
      openParen: OpenParenAST,
      preCloseParenSpace: Option[Space],
      closeParen: CloseParenAST)
    extends ParameterClauseAST

  case class NonEmptyParameterClause(
      index: SourceIndex,
      openParen: OpenParenAST,
      preParamsSpace: Option[Space],
      headParam: Parameter,
      tailParams: Seq[TailParameter],
      postParamSpace: Option[Space],
      closeParen: CloseParenAST)
    extends ParameterClauseAST

  case class TailParameter(
      index: SourceIndex,
      preCommaSpace: Option[Space],
      comma: Comma,
      postCommaSpace: Option[Space],
      parameter: Parameter)
    extends SoftAST

  case class Parameter(
      index: SourceIndex,
      paramName: IdentifierAST,
      preColonSpace: Option[Space],
      colon: ColonAST,
      postColonSpace: Option[Space],
      paramType: TypeAST)
    extends SoftAST

  case class Function(
      index: SourceIndex,
      fn: Fn,
      preSignatureSpace: SpaceAST,
      signature: FunctionSignature,
      postSignatureSpace: Option[Space],
      block: Option[BlockClause])
    extends BodyPartAST

  case class FunctionSignature(
      index: SourceIndex,
      fnName: IdentifierAST,
      preParamSpace: Option[Space],
      params: ParameterClauseAST,
      postParamSpace: Option[Space],
      returned: FunctionReturnAST)
    extends SoftAST

  sealed trait TypeAST       extends SoftAST
  sealed trait SingleTypeAST extends TypeAST

  case class Type(
      code: String,
      index: SourceIndex)
    extends SingleTypeAST
       with CodeAST

  case class TupledType(
      index: SourceIndex,
      openParen: OpenParen,
      preHeadTypeSpace: Option[Space],
      headType: TypeAST,
      tailTypes: Seq[TailType],
      closeParen: CloseParenAST)
    extends TypeAST

  case class TypeExpected(
      index: SourceIndex)
    extends ExpectedErrorAST("Type")
       with SingleTypeAST

  case class TailType(
      index: SourceIndex,
      comma: Comma,
      preTypeNameSpace: Option[Space],
      tpe: TypeAST,
      postTypeNameSpace: Option[Space])
    extends SoftAST

  sealed trait ArgumentAST       extends SoftAST     // Syntax: (arg1, arg2, (arg3, arg4))
  sealed trait NonEmptyArguments extends ArgumentAST // Does not contain `ArgumentExpected`
  sealed trait SingleArgumentAST extends ArgumentAST // Does not contain multiple arguments

  /** A named argument. Syntax: (arg1) */
  case class Argument(
      code: String,
      index: SourceIndex)
    extends SingleArgumentAST
       with NonEmptyArguments
       with CodeAST

  /** Multiple arguments. Syntax: (arg1, (arg2, arg3)) */
  case class Arguments(
      index: SourceIndex,
      openParen: OpenParenAST,
      preHeadArgumentSpace: Option[Space],
      headArgument: Option[ArgumentAST],
      tailArguments: Seq[TailArgument],
      closeParen: CloseParenAST)
    extends NonEmptyArguments
       with BodyPartAST

  /** Missing argument. Syntax: (>>missing<<, (arg2, arg3)) */
  case class ArgumentExpected(
      index: SourceIndex)
    extends ExpectedErrorAST("Argument")
       with SingleArgumentAST

  /** Syntax: (arg1, >>arg2, (arg3, arg4)<<) */
  case class TailArgument(
      index: SourceIndex,
      comma: Comma,
      preArgumentSpace: Option[Space],
      argument: ArgumentAST,
      postArgumentSpace: Option[Space])
    extends SoftAST

  sealed trait FunctionReturnAST extends SoftAST

  case class FunctionReturn(
      index: SourceIndex,
      forwardArrow: ForwardArrowAST,
      space: Option[Space],
      tpe: TypeAST)
    extends FunctionReturnAST

  case class FunctionReturnExpected(
      index: SourceIndex)
    extends TokenExpectedErrorAST(Token.ForwardArrow)
       with FunctionReturnAST

  sealed trait IdentifierAST extends SoftAST

  case class Identifier(
      code: String,
      index: SourceIndex)
    extends IdentifierAST
       with CodeAST

  case class IdentifierExpected(
      index: SourceIndex)
    extends ExpectedErrorAST("Identifier")
       with IdentifierAST

  case class Comment(
      index: SourceIndex,
      doubleForwardSlash: DoubleForwardSlash,
      space: Option[Space],
      text: Option[Text])
    extends BodyPartAST

  case class Text(
      code: String,
      index: SourceIndex)
    extends CodeAST

  case class Unresolved(
      code: String,
      index: SourceIndex)
    extends ErrorAST(s"Cannot resolve '$code'")
       with CodeAST
       with BodyPartAST

  sealed trait SpaceAST extends SoftAST

  case class Space(
      code: String,
      index: SourceIndex)
    extends SpaceAST
       with CodeAST {

    override def toStringPretty(): String =
      this
        .copy(
          code = code
            .replaceAll("\r", "\\\\r") // Replace carriage with \\r
            .replaceAll("\n", "\\\\n") // Replace newline with \\n
            .replaceAll("\t", "\\\\t") // Replace tab with \\t
        )
        .toString

  }

  case class SpaceExpected(
      index: SourceIndex)
    extends ExpectedErrorAST("Space")
       with SpaceAST

  /**
   * TODO: Currently a reference is an identifier [[IdentifierAST]].
   *       It should be replaced with an expression, when an `Expression`
   *       type is available.
   *
   * Syntax: reference(arg1, arg2, (arg3, arg4))
   */
  case class ReferenceCall(
      index: SourceIndex,
      reference: IdentifierAST,
      preArgumentsSpace: Option[Space],
      arguments: Arguments)
    extends BodyPartAST

  private def collectASTs: PartialFunction[Any, Seq[SoftAST]] = {
    case ast: SoftAST =>
      Seq(ast)

    case Some(ast: SoftAST) =>
      Seq(ast)

    case seq: Seq[_] =>
      seq flatMap collectASTs

    case _ =>
      Seq.empty
  }

}
