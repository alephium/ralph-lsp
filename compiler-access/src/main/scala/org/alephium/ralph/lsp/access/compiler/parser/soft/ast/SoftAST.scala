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
    s"${self.getClass.getSimpleName}: ${self.index}"

}

object SoftAST {

  implicit class NodeSoftASTExtensions(val node: Node[SoftAST, SoftAST]) extends AnyVal {

    def toCode(): String =
      node.walkDown.foldLeft("") {
        case (code, Node(ast: Code, _)) =>
          code + ast.text

        case (code, _) =>
          code
      }

    def toStringTree(): String =
      node.toStringTree(_.toStringPretty())

  }

  sealed trait BodyPartAST extends SoftAST

  sealed trait ExpressionAST extends BodyPartAST

  case class ExpressionExpected(
      index: SourceIndex)
    extends ExpectedErrorAST("Symbol")
       with ExpressionAST

  /**
   * ASTs that store code.
   */
  sealed trait CodeAST extends SoftAST {

    def code: Code

  }

  /**
   * [[CodeAST]] instances that can contain documentation.
   */
  sealed trait CodeDocumentedAST extends CodeAST {

    def documentation: Option[Comments]

  }

  /**
   * Has an [[Token]] type associates with it.
   */
  sealed trait TokenAST extends CodeAST

  /**
   * [[TokenAST]] instances that can contain documentation.
   */
  sealed trait TokenDocumentedAST extends TokenAST with CodeDocumentedAST

  abstract class ErrorAST(val message: String)       extends SoftAST
  abstract class ExpectedErrorAST(element: String)   extends ErrorAST(s"$element expected")
  abstract class TokenExpectedErrorAST(token: Token) extends ExpectedErrorAST(s"'${token.lexeme}'")

  object Fn {

    def apply(code: Code): Fn =
      Fn(
        index = code.index,
        documentation = None,
        code = code
      )

  }

  case class Fn(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends TokenDocumentedAST

  case class Dot(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends TokenDocumentedAST

  object Comma {

    def apply(code: Code): Comma =
      Comma(
        index = code.index,
        documentation = None,
        code = code
      )

  }

  case class Comma(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends TokenDocumentedAST

  case class Return(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends TokenDocumentedAST

  case class For(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends TokenDocumentedAST

  case class While(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends TokenDocumentedAST

  case class Equal(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends TokenDocumentedAST

  case class DoubleForwardSlash(code: Code) extends TokenAST {

    override def index: SourceIndex =
      code.index

  }

  case class Const(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends TokenDocumentedAST

  case class Pub(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends TokenDocumentedAST

  case class At(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends TokenDocumentedAST

  case class Operator(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends TokenDocumentedAST

  sealed trait InheritanceType extends SoftAST

  case class Implements(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends TokenDocumentedAST
       with InheritanceType

  case class Extends(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends TokenDocumentedAST
       with InheritanceType

  sealed trait TemplateToken extends TokenDocumentedAST

  object Contract {

    def apply(code: Code): Contract =
      Contract(
        index = code.index,
        documentation = None,
        code = code
      )

  }

  case class Contract(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends TemplateToken

  object TxScript {

    def apply(code: Code): TxScript =
      TxScript(
        index = code.index,
        documentation = None,
        code = code
      )

  }

  case class TxScript(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends TemplateToken

  sealed trait DataTemplateToken extends TokenDocumentedAST

  case class Struct(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends DataTemplateToken

  case class Enum(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends DataTemplateToken

  case class Event(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends DataTemplateToken

  sealed trait ColonAST extends SoftAST

  object Colon {

    def apply(code: Code): Colon =
      Colon(
        index = code.index,
        documentation = None,
        code = code
      )

  }

  case class Colon(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends TokenDocumentedAST
       with ColonAST

  case class ColonExpected(
      index: SourceIndex)
    extends TokenExpectedErrorAST(Token.Colon)
       with ColonAST

  sealed trait ForwardArrowAST extends SoftAST

  object ForwardArrow {

    def apply(code: Code): ForwardArrow =
      ForwardArrow(
        index = code.index,
        documentation = None,
        code = code
      )

  }

  case class ForwardArrow(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends TokenDocumentedAST
       with ForwardArrowAST

  case class ForwardArrowExpected(
      index: SourceIndex)
    extends TokenExpectedErrorAST(Token.ForwardArrow)
       with ForwardArrowAST

  sealed trait OpenParenAST extends SoftAST

  object OpenParen {

    def apply(code: Code): OpenParen =
      OpenParen(
        index = code.index,
        documentation = None,
        code = code
      )

  }

  case class OpenParen(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends TokenDocumentedAST
       with OpenParenAST

  case class OpenParenExpected(
      index: SourceIndex)
    extends TokenExpectedErrorAST(Token.OpenParen)
       with OpenParenAST

  sealed trait CloseParenAST extends SoftAST

  object CloseParen {

    def apply(code: Code): CloseParen =
      CloseParen(
        index = code.index,
        documentation = None,
        code = code
      )

  }

  case class CloseParen(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends TokenDocumentedAST
       with CloseParenAST

  case class CloseParenExpected(
      index: SourceIndex)
    extends TokenExpectedErrorAST(Token.CloseParen)
       with CloseParenAST

  sealed trait OpenCurlyAST extends SoftAST

  object OpenCurly {

    def apply(code: Code): OpenCurly =
      OpenCurly(
        index = code.index,
        documentation = None,
        code = code
      )

  }

  case class OpenCurly(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends TokenDocumentedAST
       with OpenCurlyAST

  case class OpenCurlyExpected(
      index: SourceIndex)
    extends TokenExpectedErrorAST(Token.OpenCurly)
       with OpenCurlyAST

  sealed trait CloseCurlyAST extends SoftAST

  object CloseCurly {

    def apply(code: Code): CloseCurly =
      CloseCurly(
        index = code.index,
        documentation = None,
        code = code
      )

  }

  case class CloseCurly(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends TokenDocumentedAST
       with CloseCurlyAST

  case class CloseCurlyExpected(
      index: SourceIndex)
    extends TokenExpectedErrorAST(Token.CloseCurly)
       with CloseCurlyAST

  sealed trait SemicolonAST extends SoftAST

  case class Semicolon(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends TokenDocumentedAST
       with SemicolonAST

  case class SemicolonExpected(
      index: SourceIndex)
    extends TokenExpectedErrorAST(Token.Semicolon)
       with SemicolonAST

  sealed abstract class AssignmentControlToken extends TokenDocumentedAST

  case class Let(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends AssignmentControlToken

  case class Mut(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends AssignmentControlToken

  case class Template(
      index: SourceIndex,
      templateType: TemplateToken,
      preIdentifierSpace: SpaceAST,
      identifier: IdentifierAST,
      preParamSpace: Option[Space],
      params: Option[Tuple],
      postParamSpace: Option[Space],
      inheritance: Seq[TemplateInheritance],
      block: BlockClause)
    extends BodyPartAST

  case class DataTemplate(
      index: SourceIndex,
      dataType: DataTemplateToken,
      preIdentifierSpace: SpaceAST,
      identifier: IdentifierAST,
      preParamSpace: Option[Space],
      params: Tuple)
    extends BodyPartAST

  /** Syntax: `implements or extends contract(arg1, arg2 ...)` */
  case class TemplateInheritance(
      index: SourceIndex,
      inheritanceType: InheritanceType,
      preConstructorCallSpace: SpaceAST,
      reference: ReferenceCallOrIdentifier,
      postConstructorCallSpace: Option[Space])
    extends SoftAST

  case class BlockClause(
      index: SourceIndex,
      openCurly: OpenCurlyAST,
      preBodySpace: Option[Space],
      body: BlockBody,
      postBodySpace: Option[Space],
      closeCurly: CloseCurlyAST)
    extends ExpressionAST

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

  case class Function(
      index: SourceIndex,
      annotation: Option[Annotation],
      postAnnotationSpace: Option[Space],
      pub: Option[AccessModifier],
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
      params: Tuple,
      postParamSpace: Option[Space],
      returned: FunctionReturnAST)
    extends SoftAST

  sealed trait TypeAST extends SoftAST

  /** Multiple arguments. Syntax: (arg1, (arg2, arg3)) */
  case class Tuple(
      index: SourceIndex,
      openParen: OpenParenAST,
      preHeadExpressionSpace: Option[Space],
      headExpression: Option[ExpressionAST],
      postHeadExpressionSpace: Option[Space],
      tailExpressions: Seq[TupleTail],
      closeParen: CloseParenAST)
    extends ExpressionAST
       with TypeAST

  /** Syntax: (arg1, >>arg2, (arg3, arg4)<<) */
  case class TupleTail(
      index: SourceIndex,
      comma: Comma,
      preExpressionSpace: Option[Space],
      expression: ExpressionAST,
      postExpressionSpace: Option[Space])
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

  sealed trait IdentifierAST extends TypeAST with ReferenceCallOrIdentifier

  object Identifier {

    def apply(code: Code): Identifier =
      Identifier(
        index = code.index,
        documentation = None,
        code = code
      )

  }

  case class Identifier(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends IdentifierAST
       with CodeDocumentedAST
       with ExpressionAST

  case class IdentifierExpected(
      index: SourceIndex)
    extends ExpectedErrorAST("Identifier")
       with IdentifierAST

  case class Comments(
      index: SourceIndex,
      preCommentSpace: Option[Space],
      comments: Seq[Comment],
      postCommentSpace: Option[Space])
    extends BodyPartAST

  case class Comment(
      index: SourceIndex,
      doubleForwardSlash: DoubleForwardSlash,
      preTextSpace: Option[Space],
      text: Option[Code],
      postTextSpace: Option[Space])
    extends BodyPartAST

  object Unresolved {

    def apply(code: Code): Unresolved =
      Unresolved(
        index = code.index,
        documentation = None,
        code = code
      )

  }

  case class Unresolved(
      index: SourceIndex,
      documentation: Option[Comments],
      code: Code)
    extends ErrorAST(s"Cannot resolve '$code'")
       with CodeDocumentedAST
       with BodyPartAST

  sealed trait ReferenceCallOrIdentifier extends SoftAST

  case class ReferenceCall(
      index: SourceIndex,
      reference: IdentifierAST,
      preArgumentsSpace: Option[Space],
      arguments: Tuple)
    extends ExpressionAST
       with ReferenceCallOrIdentifier

  case class InfixExpression(
      index: SourceIndex,
      leftExpression: ExpressionAST,
      preOperatorSpace: Option[Space],
      operator: Operator,
      postOperatorSpace: Option[Space],
      rightExpression: ExpressionAST)
    extends ExpressionAST

  case class MethodCall(
      index: SourceIndex,
      leftExpression: ExpressionAST,
      preDotSpace: Option[Space],
      dotCalls: Seq[DotCall])
    extends ExpressionAST

  case class DotCall(
      index: SourceIndex,
      dot: Dot,
      postDotSpace: Option[Space],
      rightExpression: ReferenceCall)
    extends SoftAST

  case class ReturnStatement(
      index: SourceIndex,
      returnToken: Return,
      preExpressionSpace: SpaceAST,
      rightExpression: ExpressionAST)
    extends ExpressionAST

  case class ForStatement(
      index: SourceIndex,
      forToken: For,
      postForSpace: Option[Space],
      openParen: OpenParenAST,
      postOpenParenSpace: Option[Space],
      expression1: ExpressionAST,
      postExpression1Space: Option[Space],
      postExpression1Semicolon: SemicolonAST,
      postExpression1SemicolonSpace: Option[Space],
      expression2: ExpressionAST,
      postExpression2Space: Option[Space],
      postExpression2Semicolon: SemicolonAST,
      postExpression2SemicolonSpace: Option[Space],
      expression3: ExpressionAST,
      postExpression3Space: Option[Space],
      closeParen: CloseParenAST,
      postCloseParenSpace: Option[SoftAST.Space],
      block: SoftAST.BlockClause)
    extends ExpressionAST

  case class WhileStatement(
      index: SourceIndex,
      whileToken: While,
      postWhileSpace: Option[Space],
      openParen: OpenParenAST,
      postOpenParenSpace: Option[Space],
      expression: ExpressionAST,
      postExpressionSpace: Option[Space],
      closeParen: CloseParenAST,
      postCloseParenSpace: Option[SoftAST.Space],
      block: SoftAST.BlockClause)
    extends ExpressionAST

  case class Assignment(
      index: SourceIndex,
      modifiers: Seq[SoftAST.AssignmentAccessModifier],
      identifier: Identifier,
      postIdentifierSpace: Option[Space],
      equalToken: Equal,
      postEqualSpace: Option[Space],
      expression: ExpressionAST)
    extends ExpressionAST

  case class TypeAssignment(
      index: SourceIndex,
      modifiers: Seq[SoftAST.AssignmentAccessModifier],
      name: IdentifierAST,
      preColonSpace: Option[Space],
      colon: ColonAST,
      postColonSpace: Option[Space],
      tpe: TypeAST)
    extends ExpressionAST

  case class AssignmentAccessModifier(
      index: SourceIndex,
      token: AssignmentControlToken,
      postTokenSpace: Option[Space])
    extends ExpressionAST

  case class AccessModifier(
      index: SourceIndex,
      pub: Pub,
      postTokenSpace: Option[Space])
    extends ExpressionAST

  case class Annotation(
      index: SourceIndex,
      at: At,
      preIdentifierSpace: Option[Space],
      identifier: IdentifierAST,
      postIdentifierSpace: Option[Space],
      tuple: Option[Tuple])
    extends ExpressionAST

  sealed trait SpaceAST extends SoftAST

  case class Space(
      code: Code)
    extends SpaceAST
       with CodeAST {

    override def index: SourceIndex =
      code.index

  }

  case class SpaceExpected(
      index: SourceIndex)
    extends ExpectedErrorAST("Space")
       with SpaceAST

  case class Code(
      index: SourceIndex,
      text: String)
    extends SoftAST {

    private def textPretty: String =
      text
        .replaceAll("\r", "\\\\r") // Replace carriage with \\r
        .replaceAll("\n", "\\\\n") // Replace newline with \\n
        .replaceAll("\t", "\\\\t") // Replace tab with \\t)

    override def toStringPretty(): String =
      s"${getClass.getSimpleName}(\"$textPretty\"): $index"

  }

  private def collectASTs: PartialFunction[Any, Seq[SoftAST]] = {
    case ast: SoftAST =>
      Seq(ast)

    case some @ Some(_) =>
      (some.iterator flatMap collectASTs).toSeq

    case seq: Seq[_] =>
      seq flatMap collectASTs

    case _ =>
      Seq.empty
  }

}
