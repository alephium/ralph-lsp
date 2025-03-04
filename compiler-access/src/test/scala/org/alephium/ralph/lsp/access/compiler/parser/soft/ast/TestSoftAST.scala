package org.alephium.ralph.lsp.access.compiler.parser.soft.ast

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.util.TestCodeUtil.{indexChunkOf, indexOf}

object TestSoftAST {

  def EqualEqual(code: String): SoftAST.TokenDocumented[Token.EqualEqual.type] =
    EqualEqual(indexOf(code))

  def EqualEqual(index: SourceIndex): SoftAST.TokenDocumented[Token.EqualEqual.type] =
    TokenDocumented(
      index = index,
      token = Token.EqualEqual
    )

  def GreaterThanOrEqual(code: String): SoftAST.TokenDocumented[Token.GreaterThanOrEqual.type] =
    GreaterThanOrEqual(indexOf(code))

  def GreaterThanOrEqual(index: SourceIndex): SoftAST.TokenDocumented[Token.GreaterThanOrEqual.type] =
    TokenDocumented(
      index = index,
      token = Token.GreaterThanOrEqual
    )

  def PlusPlus(code: String): SoftAST.TokenDocumented[Token.PlusPlus.type] =
    PlusPlus(indexOf(code))

  def PlusPlus(index: SourceIndex): SoftAST.TokenDocumented[Token.PlusPlus.type] =
    TokenDocumented(
      index = index,
      token = Token.PlusPlus
    )

  def PlusEquals(code: String): SoftAST.TokenDocumented[Token.PlusEquals.type] =
    PlusEquals(indexOf(code))

  def PlusEquals(index: SourceIndex): SoftAST.TokenDocumented[Token.PlusEquals.type] =
    TokenDocumented(
      index = index,
      token = Token.PlusEquals
    )

  def MinusEquals(code: String): SoftAST.TokenDocumented[Token.MinusEquals.type] =
    MinusEquals(indexOf(code))

  def MinusEquals(index: SourceIndex): SoftAST.TokenDocumented[Token.MinusEquals.type] =
    TokenDocumented(
      index = index,
      token = Token.MinusEquals
    )

  def LessThanOrEqual(code: String): SoftAST.TokenDocumented[Token.LessThanOrEqual.type] =
    LessThanOrEqual(indexOf(code))

  def LessThanOrEqual(index: SourceIndex): SoftAST.TokenDocumented[Token.LessThanOrEqual.type] =
    TokenDocumented(
      index = index,
      token = Token.LessThanOrEqual
    )

  def NotEqual(code: String): SoftAST.TokenDocumented[Token.NotEqual.type] =
    NotEqual(indexOf(code))

  def NotEqual(index: SourceIndex): SoftAST.TokenDocumented[Token.NotEqual.type] =
    TokenDocumented(
      index = index,
      token = Token.NotEqual
    )

  def ForwardArrow(code: String): SoftAST.TokenDocumented[Token.ForwardArrow.type] =
    ForwardArrow(indexOf(code))

  def ForwardArrow(index: SourceIndex): SoftAST.TokenDocumented[Token.ForwardArrow.type] =
    TokenDocumented(
      index = index,
      token = Token.ForwardArrow
    )

  def Or(code: String): SoftAST.TokenDocumented[Token.Or.type] =
    Or(indexOf(code))

  def Or(index: SourceIndex): SoftAST.TokenDocumented[Token.Or.type] =
    TokenDocumented(
      index = index,
      token = Token.Or
    )

  def And(code: String): SoftAST.TokenDocumented[Token.And.type] =
    And(indexOf(code))

  def And(index: SourceIndex): SoftAST.TokenDocumented[Token.And.type] =
    TokenDocumented(
      index = index,
      token = Token.And
    )

  def ShiftLeft(code: String): SoftAST.TokenDocumented[Token.ShiftLeft.type] =
    ShiftLeft(indexOf(code))

  def ShiftLeft(index: SourceIndex): SoftAST.TokenDocumented[Token.ShiftLeft.type] =
    TokenDocumented(
      index = index,
      token = Token.ShiftLeft
    )

  def ShiftRight(code: String): SoftAST.TokenDocumented[Token.ShiftRight.type] =
    ShiftRight(indexOf(code))

  def ShiftRight(index: SourceIndex): SoftAST.TokenDocumented[Token.ShiftRight.type] =
    TokenDocumented(
      index = index,
      token = Token.ShiftRight
    )

  def ModuloAddition(code: String): SoftAST.TokenDocumented[Token.ModuloAddition.type] =
    ModuloAddition(indexOf(code))

  def ModuloAddition(index: SourceIndex): SoftAST.TokenDocumented[Token.ModuloAddition.type] =
    TokenDocumented(
      index = index,
      token = Token.ModuloAddition
    )

  def ModuloSubtraction(code: String): SoftAST.TokenDocumented[Token.ModuloSubtraction.type] =
    ModuloSubtraction(indexOf(code))

  def ModuloSubtraction(index: SourceIndex): SoftAST.TokenDocumented[Token.ModuloSubtraction.type] =
    TokenDocumented(
      index = index,
      token = Token.ModuloSubtraction
    )

  def ModuloExponentiation(code: String): SoftAST.TokenDocumented[Token.ModuloExponentiation.type] =
    ModuloExponentiation(indexOf(code))

  def ModuloExponentiation(index: SourceIndex): SoftAST.TokenDocumented[Token.ModuloExponentiation.type] =
    TokenDocumented(
      index = index,
      token = Token.ModuloExponentiation
    )

  def ModuloMultiplication(code: String): SoftAST.TokenDocumented[Token.ModuloMultiplication.type] =
    ModuloMultiplication(indexOf(code))

  def ModuloMultiplication(index: SourceIndex): SoftAST.TokenDocumented[Token.ModuloMultiplication.type] =
    TokenDocumented(
      index = index,
      token = Token.ModuloMultiplication
    )

  def Exponentiation(code: String): SoftAST.TokenDocumented[Token.Exponentiation.type] =
    Exponentiation(indexOf(code))

  def Exponentiation(index: SourceIndex): SoftAST.TokenDocumented[Token.Exponentiation.type] =
    TokenDocumented(
      index = index,
      token = Token.Exponentiation
    )

  def Minus(code: String): SoftAST.TokenDocumented[Token.Minus.type] =
    Minus(indexOf(code))

  def Minus(index: SourceIndex): SoftAST.TokenDocumented[Token.Minus.type] =
    TokenDocumented(
      index = index,
      token = Token.Minus
    )

  def Plus(code: String): SoftAST.TokenDocumented[Token.Plus.type] =
    Plus(indexOf(code))

  def Plus(index: SourceIndex): SoftAST.TokenDocumented[Token.Plus.type] =
    TokenDocumented(
      index = index,
      token = Token.Plus
    )

  def Asterisk(code: String): SoftAST.TokenDocumented[Token.Asterisk.type] =
    Asterisk(indexOf(code))

  def Asterisk(index: SourceIndex): SoftAST.TokenDocumented[Token.Asterisk.type] =
    TokenDocumented(
      index = index,
      token = Token.Asterisk
    )

  def ForwardSlash(code: String): SoftAST.TokenDocumented[Token.ForwardSlash.type] =
    ForwardSlash(indexOf(code))

  def ForwardSlash(index: SourceIndex): SoftAST.TokenDocumented[Token.ForwardSlash.type] =
    TokenDocumented(
      index = index,
      token = Token.ForwardSlash
    )

  def GreaterThan(code: String): SoftAST.TokenDocumented[Token.GreaterThan.type] =
    GreaterThan(indexOf(code))

  def GreaterThan(index: SourceIndex): SoftAST.TokenDocumented[Token.GreaterThan.type] =
    TokenDocumented(
      index = index,
      token = Token.GreaterThan
    )

  def LessThan(code: String): SoftAST.TokenDocumented[Token.LessThan.type] =
    LessThan(indexOf(code))

  def LessThan(index: SourceIndex): SoftAST.TokenDocumented[Token.LessThan.type] =
    TokenDocumented(
      index = index,
      token = Token.LessThan
    )

  def Equal(code: String): SoftAST.TokenDocumented[Token.Equal.type] =
    Equal(indexOf(code))

  def Equal(index: SourceIndex): SoftAST.TokenDocumented[Token.Equal.type] =
    TokenDocumented(
      index = index,
      token = Token.Equal
    )

  def Exclamation(code: String): SoftAST.TokenDocumented[Token.Exclamation.type] =
    Exclamation(indexOf(code))

  def Exclamation(index: SourceIndex): SoftAST.TokenDocumented[Token.Exclamation.type] =
    TokenDocumented(
      index = index,
      token = Token.Exclamation
    )

  def Bar(code: String): SoftAST.TokenDocumented[Token.Bar.type] =
    Bar(indexOf(code))

  def Bar(index: SourceIndex): SoftAST.TokenDocumented[Token.Bar.type] =
    TokenDocumented(
      index = index,
      token = Token.Bar
    )

  def Ampersand(code: String): SoftAST.TokenDocumented[Token.Ampersand.type] =
    Ampersand(indexOf(code))

  def Ampersand(index: SourceIndex): SoftAST.TokenDocumented[Token.Ampersand.type] =
    TokenDocumented(
      index = index,
      token = Token.Ampersand
    )

  def Caret(code: String): SoftAST.TokenDocumented[Token.Caret.type] =
    Caret(indexOf(code))

  def Caret(index: SourceIndex): SoftAST.TokenDocumented[Token.Caret.type] =
    TokenDocumented(
      index = index,
      token = Token.Caret
    )

  def Percent(code: String): SoftAST.TokenDocumented[Token.Percent.type] =
    Percent(indexOf(code))

  def Percent(index: SourceIndex): SoftAST.TokenDocumented[Token.Percent.type] =
    TokenDocumented(
      index = index,
      token = Token.Percent
    )

  def OpenParen(code: String): SoftAST.TokenDocumented[Token.OpenParen.type] =
    OpenParen(indexOf(code))

  def OpenParen(index: SourceIndex): SoftAST.TokenDocumented[Token.OpenParen.type] =
    TokenDocumented(
      index = index,
      token = Token.OpenParen
    )

  def CloseParen(code: String): SoftAST.TokenDocumented[Token.CloseParen.type] =
    CloseParen(indexOf(code))

  def CloseParen(index: SourceIndex): SoftAST.TokenDocumented[Token.CloseParen.type] =
    TokenDocumented(
      index = index,
      token = Token.CloseParen
    )

  def OpenCurly(code: String): SoftAST.TokenDocumented[Token.OpenCurly.type] =
    OpenCurly(indexOf(code))

  def OpenCurly(index: SourceIndex): SoftAST.TokenDocumented[Token.OpenCurly.type] =
    TokenDocumented(
      index = index,
      token = Token.OpenCurly
    )

  def CloseCurly(code: String): SoftAST.TokenDocumented[Token.CloseCurly.type] =
    CloseCurly(indexOf(code))

  def CloseCurly(index: SourceIndex): SoftAST.TokenDocumented[Token.CloseCurly.type] =
    TokenDocumented(
      index = index,
      token = Token.CloseCurly
    )

  def OpenBracket(code: String): SoftAST.TokenDocumented[Token.OpenBracket.type] =
    OpenBracket(indexOf(code))

  def OpenBracket(index: SourceIndex): SoftAST.TokenDocumented[Token.OpenBracket.type] =
    TokenDocumented(
      index = index,
      token = Token.OpenBracket
    )

  def BlockBracket(code: String): SoftAST.TokenDocumented[Token.BlockBracket.type] =
    BlockBracket(indexOf(code))

  def BlockBracket(index: SourceIndex): SoftAST.TokenDocumented[Token.BlockBracket.type] =
    TokenDocumented(
      index = index,
      token = Token.BlockBracket
    )

  def Comma(code: String): SoftAST.TokenDocumented[Token.Comma.type] =
    Comma(indexOf(code))

  def Comma(index: SourceIndex): SoftAST.TokenDocumented[Token.Comma.type] =
    TokenDocumented(
      index = index,
      token = Token.Comma
    )

  def Dot(code: String): SoftAST.TokenDocumented[Token.Dot.type] =
    Dot(indexOf(code))

  def Dot(index: SourceIndex): SoftAST.TokenDocumented[Token.Dot.type] =
    TokenDocumented(
      index = index,
      token = Token.Dot
    )

  def Colon(code: String): SoftAST.TokenDocumented[Token.Colon.type] =
    Colon(indexOf(code))

  def Colon(index: SourceIndex): SoftAST.TokenDocumented[Token.Colon.type] =
    TokenDocumented(
      index = index,
      token = Token.Colon
    )

  def Semicolon(code: String): SoftAST.TokenDocumented[Token.Semicolon.type] =
    Semicolon(indexOf(code))

  def Semicolon(index: SourceIndex): SoftAST.TokenDocumented[Token.Semicolon.type] =
    TokenDocumented(
      index = index,
      token = Token.Semicolon
    )

  def DoubleForwardSlash(code: String): SoftAST.TokenUndocumented[Token.DoubleForwardSlash.type] =
    DoubleForwardSlash(indexOf(code))

  def DoubleForwardSlash(index: SourceIndex): SoftAST.TokenUndocumented[Token.DoubleForwardSlash.type] =
    SoftAST.TokenUndocumented(
      SoftAST.CodeToken(
        index = index,
        token = Token.DoubleForwardSlash
      )
    )

  def Newline(code: String): SoftAST.TokenDocumented[Token.Newline.type] =
    Newline(indexOf(code))

  def Newline(index: SourceIndex): SoftAST.TokenDocumented[Token.Newline.type] =
    TokenDocumented(
      index = index,
      token = Token.Newline
    )

  def Tab(code: String): SoftAST.TokenDocumented[Token.Tab.type] =
    Tab(indexOf(code))

  def Tab(index: SourceIndex): SoftAST.TokenDocumented[Token.Tab.type] =
    TokenDocumented(
      index = index,
      token = Token.Tab
    )

  def Hash(code: String): SoftAST.TokenDocumented[Token.Hash.type] =
    Hash(indexOf(code))

  def Hash(index: SourceIndex): SoftAST.TokenDocumented[Token.Hash.type] =
    TokenDocumented(
      index = index,
      token = Token.Hash
    )

  def Underscore(code: String): SoftAST.TokenDocumented[Token.Underscore.type] =
    Underscore(indexOf(code))

  def Underscore(index: SourceIndex): SoftAST.TokenDocumented[Token.Underscore.type] =
    TokenDocumented(
      index = index,
      token = Token.Underscore
    )

  def At(code: String): SoftAST.TokenDocumented[Token.At.type] =
    At(indexOf(code))

  def At(index: SourceIndex): SoftAST.TokenDocumented[Token.At.type] =
    TokenDocumented(
      index = index,
      token = Token.At
    )

  def Tick(code: String): SoftAST.TokenDocumented[Token.Tick.type] =
    Tick(indexOf(code))

  def Tick(index: SourceIndex): SoftAST.TokenDocumented[Token.Tick.type] =
    TokenDocumented(
      index = index,
      token = Token.Tick
    )

  def Quote(code: String): SoftAST.TokenDocumented[Token.Quote.type] =
    Quote(indexOf(code))

  def Quote(index: SourceIndex): SoftAST.TokenDocumented[Token.Quote.type] =
    TokenDocumented(
      index = index,
      token = Token.Quote
    )

  def B(code: String): SoftAST.TokenDocumented[Token.B.type] =
    B(indexOf(code))

  def B(index: SourceIndex): SoftAST.TokenDocumented[Token.B.type] =
    TokenDocumented(
      index = index,
      token = Token.B
    )

  def Const(code: String): SoftAST.TokenDocumented[Token.Const.type] =
    Const(indexOf(code))

  def Const(index: SourceIndex): SoftAST.TokenDocumented[Token.Const.type] =
    TokenDocumented(
      index = index,
      token = Token.Const
    )

  def Let(code: String): SoftAST.TokenDocumented[Token.Let.type] =
    Let(indexOf(code))

  def Let(index: SourceIndex): SoftAST.TokenDocumented[Token.Let.type] =
    TokenDocumented(
      index = index,
      token = Token.Let
    )

  def Mut(code: String): SoftAST.TokenDocumented[Token.Mut.type] =
    Mut(indexOf(code))

  def Mut(index: SourceIndex): SoftAST.TokenDocumented[Token.Mut.type] =
    TokenDocumented(
      index = index,
      token = Token.Mut
    )

  def Struct(code: String): SoftAST.TokenDocumented[Token.Struct.type] =
    Struct(indexOf(code))

  def Struct(index: SourceIndex): SoftAST.TokenDocumented[Token.Struct.type] =
    TokenDocumented(
      index = index,
      token = Token.Struct
    )

  def Enum(code: String): SoftAST.TokenDocumented[Token.Enum.type] =
    Enum(indexOf(code))

  def Enum(index: SourceIndex): SoftAST.TokenDocumented[Token.Enum.type] =
    TokenDocumented(
      index = index,
      token = Token.Enum
    )

  def Event(code: String): SoftAST.TokenDocumented[Token.Event.type] =
    Event(indexOf(code))

  def Event(index: SourceIndex): SoftAST.TokenDocumented[Token.Event.type] =
    TokenDocumented(
      index = index,
      token = Token.Event
    )

  def If(code: String): SoftAST.TokenDocumented[Token.If.type] =
    If(indexOf(code))

  def If(index: SourceIndex): SoftAST.TokenDocumented[Token.If.type] =
    TokenDocumented(
      index = index,
      token = Token.If
    )

  def Else(code: String): SoftAST.TokenDocumented[Token.Else.type] =
    Else(indexOf(code))

  def Else(index: SourceIndex): SoftAST.TokenDocumented[Token.Else.type] =
    TokenDocumented(
      index = index,
      token = Token.Else
    )

  def While(code: String): SoftAST.TokenDocumented[Token.While.type] =
    While(indexOf(code))

  def While(index: SourceIndex): SoftAST.TokenDocumented[Token.While.type] =
    TokenDocumented(
      index = index,
      token = Token.While
    )

  def Return(code: String): SoftAST.TokenDocumented[Token.Return.type] =
    Return(indexOf(code))

  def Return(index: SourceIndex): SoftAST.TokenDocumented[Token.Return.type] =
    TokenDocumented(
      index = index,
      token = Token.Return
    )

  def For(code: String): SoftAST.TokenDocumented[Token.For.type] =
    For(indexOf(code))

  def For(index: SourceIndex): SoftAST.TokenDocumented[Token.For.type] =
    TokenDocumented(
      index = index,
      token = Token.For
    )

  def Pub(code: String): SoftAST.TokenDocumented[Token.Pub.type] =
    Pub(indexOf(code))

  def Pub(index: SourceIndex): SoftAST.TokenDocumented[Token.Pub.type] =
    TokenDocumented(
      index = index,
      token = Token.Pub
    )

  def Fn(code: String): SoftAST.TokenDocumented[Token.Fn.type] =
    Fn(indexOf(code))

  def Fn(index: SourceIndex): SoftAST.TokenDocumented[Token.Fn.type] =
    TokenDocumented(
      index = index,
      token = Token.Fn
    )

  def Import(code: String): SoftAST.TokenDocumented[Token.Import.type] =
    Import(indexOf(code))

  def Import(index: SourceIndex): SoftAST.TokenDocumented[Token.Import.type] =
    TokenDocumented(
      index = index,
      token = Token.Import
    )

  def Abstract(code: String): SoftAST.TokenDocumented[Token.Abstract.type] =
    Abstract(indexOf(code))

  def Abstract(index: SourceIndex): SoftAST.TokenDocumented[Token.Abstract.type] =
    TokenDocumented(
      index = index,
      token = Token.Abstract
    )

  def Contract(code: String): SoftAST.TokenDocumented[Token.Contract.type] =
    Contract(indexOf(code))

  def Contract(index: SourceIndex): SoftAST.TokenDocumented[Token.Contract.type] =
    TokenDocumented(
      index = index,
      token = Token.Contract
    )

  def TxScript(code: String): SoftAST.TokenDocumented[Token.TxScript.type] =
    TxScript(indexOf(code))

  def TxScript(index: SourceIndex): SoftAST.TokenDocumented[Token.TxScript.type] =
    TokenDocumented(
      index = index,
      token = Token.TxScript
    )

  def AssetScript(code: String): SoftAST.TokenDocumented[Token.AssetScript.type] =
    AssetScript(indexOf(code))

  def AssetScript(index: SourceIndex): SoftAST.TokenDocumented[Token.AssetScript.type] =
    TokenDocumented(
      index = index,
      token = Token.AssetScript
    )

  def Interface(code: String): SoftAST.TokenDocumented[Token.Interface.type] =
    Interface(indexOf(code))

  def Interface(index: SourceIndex): SoftAST.TokenDocumented[Token.Interface.type] =
    TokenDocumented(
      index = index,
      token = Token.Interface
    )

  def Extends(code: String): SoftAST.TokenDocumented[Token.Extends.type] =
    Extends(indexOf(code))

  def Extends(index: SourceIndex): SoftAST.TokenDocumented[Token.Extends.type] =
    TokenDocumented(
      index = index,
      token = Token.Extends
    )

  def Implements(code: String): SoftAST.TokenDocumented[Token.Implements.type] =
    Implements(indexOf(code))

  def Implements(index: SourceIndex): SoftAST.TokenDocumented[Token.Implements.type] =
    TokenDocumented(
      index = index,
      token = Token.Implements
    )

  def Using(code: String): SoftAST.TokenDocumented[Token.Using.type] =
    Using(indexOf(code))

  def Using(index: SourceIndex): SoftAST.TokenDocumented[Token.Using.type] =
    TokenDocumented(
      index = index,
      token = Token.Using
    )

  def Emit(code: String): SoftAST.TokenDocumented[Token.Emit.type] =
    Emit(indexOf(code))

  def Emit(index: SourceIndex): SoftAST.TokenDocumented[Token.Emit.type] =
    TokenDocumented(
      index = index,
      token = Token.Emit
    )

  def Embeds(code: String): SoftAST.TokenDocumented[Token.Embeds.type] =
    Embeds(indexOf(code))

  def Embeds(index: SourceIndex): SoftAST.TokenDocumented[Token.Embeds.type] =
    TokenDocumented(
      index = index,
      token = Token.Embeds
    )

  def True(code: String): SoftAST.TokenDocumented[Token.True.type] =
    True(indexOf(code))

  def True(index: SourceIndex): SoftAST.TokenDocumented[Token.True.type] =
    TokenDocumented(
      index = index,
      token = Token.True
    )

  def False(code: String): SoftAST.TokenDocumented[Token.False.type] =
    False(indexOf(code))

  def False(index: SourceIndex): SoftAST.TokenDocumented[Token.False.type] =
    TokenDocumented(
      index = index,
      token = Token.False
    )

  def AlphLowercase(code: String): SoftAST.TokenDocumented[Token.AlphLowercase.type] =
    AlphLowercase(indexOf(code))

  def AlphLowercase(index: SourceIndex): SoftAST.TokenDocumented[Token.AlphLowercase.type] =
    TokenDocumented(
      index = index,
      token = Token.AlphLowercase
    )

  def AlphUppercase(code: String): SoftAST.TokenDocumented[Token.AlphUppercase.type] =
    AlphUppercase(indexOf(code))

  def AlphUppercase(index: SourceIndex): SoftAST.TokenDocumented[Token.AlphUppercase.type] =
    TokenDocumented(
      index = index,
      token = Token.AlphUppercase
    )

  def Identifier(code: String): SoftAST.Identifier =
    Identifier(indexChunkOf(code))

  def Identifier(indexCode: (SourceIndex, String)): SoftAST.Identifier =
    Identifier(
      index = indexCode._1,
      text = indexCode._2
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

  def Number(code: String): SoftAST.Number =
    Number(indexChunkOf(code))

  def Number(indexCode: (SourceIndex, String)): SoftAST.Number =
    Number(
      index = indexCode._1,
      text = indexCode._2
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
      unit = None
    )

  def Unresolved(code: String): SoftAST.Unresolved =
    Unresolved(indexChunkOf(code))

  def Unresolved(indexCode: (SourceIndex, String)): SoftAST.Unresolved =
    Unresolved(
      index = indexCode._1,
      text = indexCode._2
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

  def IdentifierExpected(code: String): SoftAST.IdentifierExpected =
    SoftAST.IdentifierExpected(indexOf(code))

  def ExpressionExpected(code: String): SoftAST.ExpressionExpected =
    SoftAST.ExpressionExpected(indexOf(code))

  def TokenExpected[T <: Token](
      code: String,
      token: T): SoftAST.TokenExpected[T] =
    SoftAST.TokenExpected(
      index = indexOf(code),
      token = token
    )

  def Space(text: String): SoftAST.Space =
    Space(indexChunkOf(text))

  def Space(indexText: (SourceIndex, String)): SoftAST.Space =
    Space(
      index = indexText._1,
      text = indexText._2
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

  def Code(
      index: SourceIndex,
      token: Token): SoftAST.CodeString =
    SoftAST.CodeString(
      index = index,
      text = token.lexeme
    )

  def CodeString(text: String): SoftAST.CodeString =
    CodeString(indexChunkOf(text))

  def CodeString(indexText: (SourceIndex, String)): SoftAST.CodeString =
    CodeString(
      index = indexText._1,
      text = indexText._2
    )

  def CodeString(
      index: SourceIndex,
      text: String): SoftAST.CodeString =
    SoftAST.CodeString(
      index = index,
      text = text
    )

  def CodeStringExpected(text: String): SoftAST.CodeStringExpected =
    SoftAST.CodeStringExpected(indexOf(text))

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
