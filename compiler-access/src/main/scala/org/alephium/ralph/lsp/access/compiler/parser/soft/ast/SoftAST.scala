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
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST.collectASTs
import org.alephium.ralph.lsp.utils.Node

sealed trait SoftAST extends Product { self =>

  def index: SourceIndex

  final def children(): Iterator[SoftAST] =
    productIterator flatMap collectASTs

  /**
   * Similar to [[org.alephium.ralph.lsp.access.compiler.ast.Tree.Source.rootNode]],
   * this tree creation is also lazily evaluated and expected to have concurrent access.
   *
   * TODO: Move these caches to solutions like Caffeine.
   */
  final lazy val toNode: Node[this.type, SoftAST] =
    Node(
      data = self,
      children = children().map(_.toNode).toSeq
    )

  final def toCode(): String =
    toNode.toCode()

  final def toStringTree(): String =
    toNode.toStringTree()

  def toStringPretty(): String =
    s"${self.getClass.getSimpleName}: ${self.index}"

  /**
   * Checks if `this` AST's position is before the `anchor` AST's position.
   *
   * @param anchor The AST with which the position of `current` is compared.
   * @return `true` if `current`'s position is before `anchor`'s position, `false` otherwise.
   */
  def isBehind(anchor: SoftAST): Boolean =
    this.index isBehind anchor.index

  def contains(anchor: SoftAST): Boolean =
    contains(anchor.index)

  def contains(anchor: SourceIndex): Boolean =
    this.index containsSoft anchor

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

    def contains(anchor: Node[SoftAST, SoftAST]): Boolean =
      node.data contains anchor.data

  }

  sealed trait BlockPartAST extends SoftAST

  sealed trait ExpressionAST extends BlockPartAST

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
  sealed trait TokenAST[+T <: Token] extends CodeAST {

    def code: CodeToken[T]

  }

  abstract class ErrorAST(val message: String) extends SoftAST {

    // Also display the error message in the Tree representation
    final override def toStringPretty(): String =
      s"""${this.getClass.getSimpleName}("$message"): ${this.index}"""

  }

  abstract class ExpectedErrorAST(element: String)   extends ErrorAST(s"$element expected")
  abstract class TokenExpectedErrorAST(token: Token) extends ExpectedErrorAST(s"'${token.lexeme}'")

  sealed trait TokenDocExpectedAST[+T <: Token] extends SoftAST

  /**
   * Represents a token that may contain code comments or documentation.
   *
   * @param index         Source index of the token, including its documentation.
   * @param documentation Optional documentation (e.g. comments) preceding the token
   * @param code          The token itself.
   * @tparam T The specific type of token.
   */
  case class TokenDocumented[+T <: Token](
      index: SourceIndex,
      documentation: Option[Comments],
      code: CodeToken[T])
    extends TokenDocExpectedAST[T]
       with CodeDocumentedAST

  case class TokenUndocumented[+T <: Token](code: CodeToken[T]) extends TokenAST[T] {

    override def index: SourceIndex =
      code.index

  }

  case class TokenExpected[T <: Token](
      index: SourceIndex,
      token: T)
    extends TokenExpectedErrorAST(token)
       with TokenDocExpectedAST[T]

  /**
   * Represents a token that is also an expression, e.g. `true` & `false`.
   *
   * @param token A token that is also an expression.
   * @tparam T Type of token.
   */
  case class TokenExpression[+T <: Token.Expression](token: TokenDocumented[T]) extends ExpressionAST {

    override def index: SourceIndex =
      token.index

  }

  case class Template(
      index: SourceIndex,
      abstracted: Option[Abstract],
      templateType: TokenDocumented[Token.TemplateDefinition],
      preIdentifierSpace: Option[Space],
      identifier: IdentifierAST,
      preParamSpace: Option[Space],
      params: Option[Group[Token.OpenParen.type, Token.CloseParen.type]],
      postParamSpace: Option[Space],
      inheritance: Seq[Inheritance],
      block: Option[Block])
    extends BlockPartAST

  case class Abstract(
      index: SourceIndex,
      abstractToken: TokenDocumented[Token.Abstract.type],
      postAbstractSpace: Option[Space])
    extends SoftAST

  case class Event(
      index: SourceIndex,
      eventToken: TokenDocumented[Token.Event.type],
      preIdentifierSpace: Option[Space],
      identifier: IdentifierAST,
      preParamSpace: Option[Space],
      params: Group[Token.OpenParen.type, Token.CloseParen.type])
    extends BlockPartAST

  case class Struct(
      index: SourceIndex,
      structToken: TokenDocumented[Token.Struct.type],
      preIdentifierSpace: Option[Space],
      identifier: IdentifierAST,
      preParamSpace: Option[Space],
      params: Group[Token.OpenCurly.type, Token.CloseCurly.type])
    extends BlockPartAST

  case class Enum(
      index: SourceIndex,
      enumToken: TokenDocumented[Token.Enum.type],
      preIdentifierSpace: Option[Space],
      identifier: IdentifierAST,
      preBlockSpace: Option[Space],
      block: Option[Block])
    extends BlockPartAST

  case class Const(
      index: SourceIndex,
      constToken: TokenDocumented[Token.Const.type],
      preAssignmentSpace: Option[Space],
      assignment: Assignment)
    extends BlockPartAST

  case class Import(
      index: SourceIndex,
      importToken: TokenDocumented[Token.Import.type],
      postImportSpace: Option[Space],
      string: Option[StringLiteral])
    extends BlockPartAST

  /** Syntax: `implements or extends contract(arg1, arg2 ...)` */
  case class Inheritance(
      index: SourceIndex,
      inheritanceType: TokenDocumented[Token.Inheritance],
      postInheritanceTypeSpace: Option[Space],
      headReference: ReferenceCallOrIdentifier,
      tailReferencesOrSpace: Option[Either[Space, Seq[TailReferences]]])
    extends BlockPartAST {

    /** Collects all inheritance references defined. */
    def references: Seq[ReferenceCallOrIdentifier] =
      tailReferencesOrSpace match {
        case Some(Right(reference)) =>
          headReference +: reference.map(_.reference)

        case Some(Left(_: Space)) | None =>
          Seq(headReference)
      }

  }

  case class TailReferences(
      index: SourceIndex,
      comma: TokenDocumented[Token.Comma.type],
      postCommaSpace: Option[Space],
      reference: ReferenceCallOrIdentifier,
      postReferenceSpace: Option[Space])
    extends SoftAST

  /**
   * Represents an AST that contains a sequence of [[BlockPartAST]].
   */
  sealed trait BlockAST extends ExpressionAST {

    def parts: Seq[BlockPartAST]

    /**
     * Filters out empty or whitespace block-parts, i.e. block-parts of type [[Space]].
     */
    def partsNonEmpty: Seq[BlockPartAST] =
      parts.filterNot(_.isInstanceOf[SoftAST.Space])

  }

  /**
   * A [[RootBlock]] is similar to [[Block]] without the enclosing curly-braces.
   */
  case class RootBlock(
      index: SourceIndex,
      parts: Seq[BlockPartAST])
    extends BlockAST

  /**
   * Represents a code block enclosed within curly braces `{}`.
   */
  case class Block(
      index: SourceIndex,
      openCurly: TokenDocExpectedAST[Token.OpenCurly.type],
      parts: Seq[BlockPartAST],
      closeCurly: TokenDocExpectedAST[Token.CloseCurly.type])
    extends BlockAST

  case class ExpressionBlock(
      index: SourceIndex,
      headExpression: ExpressionAST,
      tailExpressions: Seq[TailExpressionBlock])
    extends BlockPartAST {

    def expressions: Seq[ExpressionAST] =
      headExpression +: tailExpressions.map(_.expression)

  }

  case class TailExpressionBlock(
      index: SourceIndex,
      preExpressionSpace: Option[Space],
      expression: ExpressionAST)
    extends SoftAST

  case class Function(
      index: SourceIndex,
      annotations: Seq[Annotation],
      accessModifier: Option[AccessModifier],
      fn: TokenDocumented[Token.Fn.type],
      preSignatureSpace: Option[Space],
      signature: FunctionSignature,
      postSignatureSpace: Option[Space],
      block: Option[Block])
    extends BlockPartAST

  case class FunctionSignature(
      index: SourceIndex,
      fnName: IdentifierAST,
      preParamSpace: Option[Space],
      params: Group[Token.OpenParen.type, Token.CloseParen.type],
      postParamSpace: Option[Space],
      returned: FunctionReturnAST)
    extends SoftAST

  /**
   * Represents a comma separated list of expressions.
   *
   * This list of expressions is parsed by the following syntax:
   *  - Struct `{ a, b: Type, mut c: C }`
   *  - Annotation `@using(a = b)`
   *  - Contract | TxScript`(a, b, c)`
   *  - fn `(a: Type, b: Type, c)`
   *  - `enum` etc
   */
  case class Group[O <: Token, C <: Token](
      index: SourceIndex,
      openToken: Option[TokenDocExpectedAST[O]],
      preHeadExpressionSpace: Option[Space],
      headExpression: Option[ExpressionAST],
      postHeadExpressionSpace: Option[Space],
      tailExpressions: Seq[GroupTail],
      closeToken: Option[TokenDocExpectedAST[C]])
    extends ExpressionAST {

    /** Collects all expressions defined in this group */
    def expressions: Iterable[ExpressionAST] =
      headExpression ++ tailExpressions.map(_.expression)

  }

  /** Comma separated tail expressions of a [[Group]] */
  case class GroupTail(
      index: SourceIndex,
      comma: TokenDocumented[Token.Comma.type],
      preExpressionSpace: Option[Space],
      expression: ExpressionAST,
      postExpressionSpace: Option[Space])
    extends SoftAST

  sealed trait FunctionReturnAST extends SoftAST

  case class FunctionReturn(
      index: SourceIndex,
      forwardArrow: TokenDocExpectedAST[Token.ForwardArrow.type],
      space: Option[Space],
      tpe: ExpressionAST)
    extends FunctionReturnAST

  case class FunctionReturnExpected(
      index: SourceIndex)
    extends TokenExpectedErrorAST(Token.ForwardArrow)
       with FunctionReturnAST

  sealed trait IdentifierAST extends ReferenceCallOrIdentifier {

    /** Converts this instance to an [[Option]], returning the error as [[None]] */
    def toOption: Option[Identifier] =
      this match {
        case id: Identifier =>
          Some(id)

        case _: IdentifierExpected =>
          None
      }

  }

  case class Identifier(
      index: SourceIndex,
      documentation: Option[Comments],
      code: CodeString)
    extends IdentifierAST
       with ExpressionAST
       with CodeDocumentedAST

  case class IdentifierExpected(
      index: SourceIndex)
    extends ExpectedErrorAST("Identifier")
       with IdentifierAST

  case class Comments(
      index: SourceIndex,
      preCommentSpace: Option[Space],
      comments: Seq[Comment],
      postCommentSpace: Option[Space])
    extends BlockPartAST

  case class Comment(
      index: SourceIndex,
      doubleForwardSlash: TokenUndocumented[Token.DoubleForwardSlash.type],
      preTextSpace: Option[Space],
      text: Option[CodeString],
      postTextSpace: Option[Space])
    extends BlockPartAST

  case class Unresolved(
      index: SourceIndex,
      documentation: Option[Comments],
      code: CodeString)
    extends ErrorAST(s"Cannot resolve '${code.text}'")
       with CodeDocumentedAST
       with BlockPartAST

  sealed trait ReferenceCallOrIdentifier extends SoftAST {

    def identifier: IdentifierAST =
      this match {
        case id: IdentifierAST =>
          id

        case call: ReferenceCall =>
          call.reference
      }

  }

  case class ReferenceCall(
      index: SourceIndex,
      reference: IdentifierAST,
      preArgumentsSpace: Option[Space],
      arguments: Group[Token.OpenParen.type, Token.CloseParen.type])
    extends ReferenceCallOrIdentifier
       with ExpressionAST

  case class InfixExpression(
      index: SourceIndex,
      leftExpression: ExpressionAST,
      preOperatorSpace: Option[Space],
      operator: TokenDocumented[Token.InfixOperator],
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
      dot: TokenDocumented[Token.Dot.type],
      postDotSpace: Option[Space],
      rightExpression: ExpressionAST)
    extends SoftAST

  case class Return(
      index: SourceIndex,
      returnToken: TokenDocumented[Token.Return.type],
      preExpressionSpace: Option[Space],
      rightExpression: ExpressionAST)
    extends ExpressionAST

  case class For(
      index: SourceIndex,
      forToken: TokenDocumented[Token.For.type],
      postForSpace: Option[Space],
      openParen: TokenDocExpectedAST[Token.OpenParen.type],
      postOpenParenSpace: Option[Space],
      expression1: ExpressionAST,
      postExpression1Space: Option[Space],
      postExpression1Semicolon: TokenDocExpectedAST[Token.Semicolon.type],
      postExpression1SemicolonSpace: Option[Space],
      expression2: ExpressionAST,
      postExpression2Space: Option[Space],
      postExpression2Semicolon: TokenDocExpectedAST[Token.Semicolon.type],
      postExpression2SemicolonSpace: Option[Space],
      expression3: ExpressionAST,
      postExpression3Space: Option[Space],
      closeParen: TokenDocExpectedAST[Token.CloseParen.type],
      postCloseParenSpace: Option[Space],
      block: Option[Block])
    extends ExpressionAST

  case class While(
      index: SourceIndex,
      whileToken: TokenDocumented[Token.While.type],
      postWhileSpace: Option[Space],
      openParen: TokenDocExpectedAST[Token.OpenParen.type],
      postOpenParenSpace: Option[Space],
      expression: ExpressionAST,
      postExpressionSpace: Option[Space],
      closeParen: TokenDocExpectedAST[Token.CloseParen.type],
      postCloseParenSpace: Option[Space],
      block: Option[Block])
    extends ExpressionAST

  case class Assignment(
      index: SourceIndex,
      expressionLeft: ExpressionAST,
      postIdentifierSpace: Option[Space],
      equalToken: TokenDocExpectedAST[Token.Equal.type],
      postEqualSpace: Option[Space],
      expressionRight: ExpressionAST)
    extends ExpressionAST

  case class TypeAssignment(
      index: SourceIndex,
      annotations: Seq[Annotation],
      expressionLeft: ExpressionAST,
      preColonSpace: Option[Space],
      colon: TokenDocumented[Token.Colon.type],
      postColonSpace: Option[Space],
      expressionRight: ExpressionAST)
    extends ExpressionAST

  case class AccessModifier(
      index: SourceIndex,
      pub: TokenDocumented[Token.Pub.type],
      postTokenSpace: Option[Space])
    extends ExpressionAST

  case class VariableDeclaration(
      index: SourceIndex,
      let: TokenDocumented[Token.Let.type],
      postLetSpace: Option[Space],
      assignment: Assignment)
    extends ExpressionAST

  /** Syntax: mut [identifier] */
  case class MutableBinding(
      index: SourceIndex,
      mut: TokenDocumented[Token.Mut.type],
      space: Option[Space],
      identifier: IdentifierAST)
    extends ExpressionAST

  case class Annotation(
      index: SourceIndex,
      at: TokenDocumented[Token.At.type],
      preIdentifierSpace: Option[Space],
      identifier: IdentifierAST,
      postIdentifierSpace: Option[Space],
      tuple: Option[Group[Token.OpenParen.type, Token.CloseParen.type]],
      postTupleSpace: Option[Space])
    extends ExpressionAST

  case class Number(
      index: SourceIndex,
      documentation: Option[Comments],
      number: CodeString,
      unit: Option[UnitAlph])
    extends ExpressionAST

  case class UnitAlph(
      index: SourceIndex,
      space: Option[Space],
      unit: TokenDocumented[Token.AlphLowercase.type])
    extends SoftAST

  case class BString(
      index: SourceIndex,
      b: TokenDocumented[Token.B.type],
      postBSpace: Option[Space],
      startTick: TokenDocumented[Token.Tick.type],
      text: Option[CodeString],
      endTick: TokenDocExpectedAST[Token.Tick.type])
    extends ExpressionAST

  case class StringLiteral(
      index: SourceIndex,
      startQuote: TokenDocumented[Token.Quote.type],
      head: Option[CodeStringAST],
      tail: Seq[Path],
      endQuote: TokenDocExpectedAST[Token.Quote.type])
    extends ExpressionAST

  case class Path(
      index: SourceIndex,
      slash: TokenDocumented[Token.ForwardSlash.type],
      text: CodeStringAST)
    extends SoftAST

  case class Else(
      index: SourceIndex,
      elseToken: TokenDocumented[Token.Else.type],
      preBlockSpace: Option[Space],
      block: Option[Block])
    extends ExpressionAST

  case class Space(
      code: CodeString)
    extends CodeAST
       with BlockPartAST {

    override def index: SourceIndex =
      code.index

  }

  sealed trait Code extends SoftAST {

    def text: String

    private def textPretty: String =
      text
        .replaceAll("\r", "\\\\r") // Replace carriage with \\r
        .replaceAll("\n", "\\\\n") // Replace newline with \\n
        .replaceAll("\t", "\\\\t") // Replace tab with \\t)

    override def toStringPretty(): String =
      s"${getClass.getSimpleName}(\"$textPretty\"): $index"

  }

  sealed trait CodeStringAST extends SoftAST

  /**
   * Represents a string within a segment of code.
   *
   * @param index Source index of the code string.
   * @param text  String content of the code
   */
  case class CodeString(
      index: SourceIndex,
      text: String)
    extends CodeStringAST
       with Code

  /** Represents a location where a code symbol is expected, but an empty value was provided. */
  case class CodeStringExpected(
      index: SourceIndex)
    extends ExpectedErrorAST("Symbol")
       with CodeStringAST

  /**
   * Represents a token within a segment of code.
   *
   * @param index Source index of the token
   * @param token Token instance representing the parsed code
   * @tparam T Specific type of token.
   */
  case class CodeToken[+T <: Token](
      index: SourceIndex,
      token: T)
    extends Code {

    override def text: String =
      token.lexeme

  }

  private def collectASTs: PartialFunction[Any, Seq[SoftAST]] = {
    case ast: SoftAST =>
      Seq(ast)

    case Some(any) =>
      collectASTs(any)

    case Left(any) =>
      collectASTs(any)

    case Right(any) =>
      collectASTs(any)

    case seq: Seq[_] =>
      seq flatMap collectASTs

    case _ =>
      Seq.empty
  }

}
