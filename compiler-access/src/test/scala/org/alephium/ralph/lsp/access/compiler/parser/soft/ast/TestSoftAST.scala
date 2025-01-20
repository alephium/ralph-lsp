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

  def Colon(index: SourceIndex): SoftAST.TokenDocumented[Token.Colon.type] =
    TokenDocumented(
      index = index,
      token = Token.Colon
    )

  def ForwardArrow(index: SourceIndex): SoftAST.TokenDocumented[Token.ForwardArrow.type] =
    TokenDocumented(
      index = index,
      token = Token.ForwardArrow
    )

  def OpenParen(index: SourceIndex): SoftAST.TokenDocumented[Token.OpenParen.type] =
    TokenDocumented(
      index = index,
      token = Token.OpenParen
    )

  def CloseParen(index: SourceIndex): SoftAST.TokenDocumented[Token.CloseParen.type] =
    TokenDocumented(
      index = index,
      token = Token.CloseParen
    )

  def OpenCurly(index: SourceIndex): SoftAST.TokenDocumented[Token.OpenCurly.type] =
    TokenDocumented(
      index = index,
      token = Token.OpenCurly
    )

  def CloseCurly(index: SourceIndex): SoftAST.TokenDocumented[Token.CloseCurly.type] =
    TokenDocumented(
      index = index,
      token = Token.CloseCurly
    )

  def True(index: SourceIndex): SoftAST.TokenDocumented[Token.True.type] =
    TokenDocumented(
      index = index,
      token = Token.True
    )

  def False(index: SourceIndex): SoftAST.TokenDocumented[Token.False.type] =
    TokenDocumented(
      index = index,
      token = Token.False
    )

  def AlphLowercase(index: SourceIndex): SoftAST.TokenDocumented[Token.AlphLowercase.type] =
    TokenDocumented(
      index = index,
      token = Token.AlphLowercase
    )

  def AlphUppercase(index: SourceIndex): SoftAST.TokenDocumented[Token.AlphUppercase.type] =
    TokenDocumented(
      index = index,
      token = Token.AlphUppercase
    )

  def Let(index: SourceIndex): SoftAST.TokenDocumented[Token.Let.type] =
    TokenDocumented(
      index = index,
      token = Token.Let
    )

  def Mut(index: SourceIndex): SoftAST.TokenDocumented[Token.Mut.type] =
    TokenDocumented(
      index = index,
      token = Token.Mut
    )

  def Equal(index: SourceIndex): SoftAST.TokenDocumented[Token.Equal.type] =
    TokenDocumented(
      index = index,
      token = Token.Equal
    )

  def Plus(index: SourceIndex): SoftAST.TokenDocumented[Token.Plus.type] =
    TokenDocumented(
      index = index,
      token = Token.Plus
    )

  def B(index: SourceIndex): SoftAST.TokenDocumented[Token.B.type] =
    TokenDocumented(
      index = index,
      token = Token.B
    )

  def Tick(index: SourceIndex): SoftAST.TokenDocumented[Token.Tick.type] =
    TokenDocumented(
      index = index,
      token = Token.Tick
    )

  def Event(index: SourceIndex): SoftAST.TokenDocumented[Token.Event.type] =
    TokenDocumented(
      index = index,
      token = Token.Event
    )

  def Struct(index: SourceIndex): SoftAST.TokenDocumented[Token.Struct.type] =
    TokenDocumented(
      index = index,
      token = Token.Struct
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

  def Number(
      index: SourceIndex,
      text: String): SoftAST.Number =
    SoftAST.Number(
      index = index,
      documentation = None,
      number = SoftAST.CodeString(
        index = index,
        text = text
      ),
      space = None,
      unit = None
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
