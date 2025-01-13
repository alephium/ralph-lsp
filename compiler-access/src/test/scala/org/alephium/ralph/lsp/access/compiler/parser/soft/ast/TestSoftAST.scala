package org.alephium.ralph.lsp.access.compiler.parser.soft.ast

import org.alephium.ralph.SourceIndex

object TestSoftAST {

  def Fn(index: SourceIndex): SoftAST.TokenDocumented[Token.Fn.type] =
    TokenDocumented(
      index = index,
      token = Token.Fn
    )

  def Comma(index: SourceIndex): SoftAST.TokenDocumented[Token.Comma.type] =
    TokenDocumented(
      index = index,
      token = Token.Comma
    )

  def DoubleForwardSlash(index: SourceIndex): SoftAST.TokenUndocumented[Token.DoubleForwardSlash.type] =
    SoftAST.TokenUndocumented(
      SoftAST.CodeToken(
        index = index,
        token = Token.DoubleForwardSlash
      )
    )

  def At(index: SourceIndex): SoftAST.TokenDocumented[Token.At.type] =
    TokenDocumented(
      index = index,
      token = Token.At
    )

  def Contract(index: SourceIndex): SoftAST.TokenDocumented[Token.Contract.type] =
    TokenDocumented(
      index = index,
      token = Token.Contract
    )

  def TxScript(index: SourceIndex): SoftAST.TokenDocumented[Token.TxScript.type] =
    TokenDocumented(
      index = index,
      token = Token.TxScript
    )

  def Colon(index: SourceIndex): SoftAST.Colon =
    SoftAST.Colon(
      index = index,
      documentation = None,
      code = Code(
        index = index,
        token = Token.Colon
      )
    )

  def ForwardArrow(index: SourceIndex): SoftAST.ForwardArrow =
    SoftAST.ForwardArrow(
      index = index,
      documentation = None,
      code = Code(
        index = index,
        token = Token.ForwardArrow
      )
    )

  def OpenParen(index: SourceIndex): SoftAST.OpenParen =
    SoftAST.OpenParen(
      index = index,
      documentation = None,
      code = Code(
        index = index,
        token = Token.OpenParen
      )
    )

  def CloseParen(index: SourceIndex): SoftAST.CloseParen =
    SoftAST.CloseParen(
      index = index,
      documentation = None,
      code = Code(
        index = index,
        token = Token.CloseParen
      )
    )

  def OpenCurly(index: SourceIndex): SoftAST.OpenCurly =
    SoftAST.OpenCurly(
      index = index,
      documentation = None,
      code = Code(
        index = index,
        token = Token.OpenCurly
      )
    )

  def CloseCurly(index: SourceIndex): SoftAST.CloseCurly =
    SoftAST.CloseCurly(
      index = index,
      documentation = None,
      code = Code(
        index = index,
        token = Token.CloseCurly
      )
    )

  def Identifier(
      index: SourceIndex,
      text: String): SoftAST.Identifier =
    SoftAST.Identifier(
      index = index,
      documentation = None,
      code = SoftAST.CodeString(
        index = index,
        text = text
      )
    )

  def Unresolved(
      index: SourceIndex,
      text: String): SoftAST.Unresolved =
    SoftAST.Unresolved(
      index = index,
      documentation = None,
      code = SoftAST.CodeString(
        index = index,
        text = text
      )
    )

  def Space(
      index: SourceIndex,
      text: String): SoftAST.Space =
    SoftAST.Space(
      code = SoftAST.CodeString(
        index = index,
        text = text
      )
    )

  def SpaceOne(index: SourceIndex): SoftAST.Space =
    SoftAST.Space(
      code = SoftAST.CodeString(
        index = index,
        text = " "
      )
    )

  def SpaceNewline(index: SourceIndex): SoftAST.Space =
    SoftAST.Space(
      code = Code(
        index = index,
        token = Token.Newline
      )
    )

  def Code(
      index: SourceIndex,
      token: Token): SoftAST.CodeString =
    SoftAST.CodeString(
      index = index,
      text = token.lexeme
    )

  def TokenDocumented[T <: Token](
      index: SourceIndex,
      token: T): SoftAST.TokenDocumented[T] =
    SoftAST.TokenDocumented(
      index = index,
      documentation = None,
      code = SoftAST.CodeToken(
        index = index,
        token = token
      )
    )

}
