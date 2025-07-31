// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft.ast

import org.alephium.macros.EnumerationMacros

sealed trait Token extends Ordered[Token] { self =>

  def lexeme: String

  /**
   * Fetches all other reserved token that may have this token's lexeme its prefix.
   *
   * For example, if this token is `-`, fetch `->` and `-=`.
   */
  lazy val otherReservedTokensWithThisPrefix: List[Token.Reserved] =
    // FIXME: All tokens should be searched instead of reserved.
    //        `sealedInstancesOf` does not work on `Token` yet.
    Token
      .reserved
      .filter {
        token =>
          token != self &&
          token.lexeme.startsWith(lexeme)
      }
      .toList

  /**
   * Order such that tokens such as:
   *  - `||` comes before `|`
   *  - `+=` comes before `+` and `=`
   */
  override def compare(that: Token): Int = {
    val lengthCompare =
      that.lexeme.length compare this.lexeme.length

    if (lengthCompare == 0)
      that.lexeme compare this.lexeme
    else
      lengthCompare
  }

}

object Token {

  /**
   * Represents tokens that are specifically reserved for use in Ralph's grammar.
   *
   * These tokens cannot be used as identifier [[SoftAST.Identifier]].
   */
  sealed trait Reserved      extends Token
  sealed trait Space         extends Token
  sealed trait InfixOperator extends Token
  sealed trait Expression    extends Token // A token that is also an expression.
  sealed trait Unary         extends Token

  sealed abstract class Operator(override val lexeme: String) extends Token
  case object EqualEqual                                      extends Operator("==") with Reserved with InfixOperator
  case object GreaterThanOrEqual                              extends Operator(">=") with Reserved with InfixOperator
  case object PlusPlus                                        extends Operator("++") with Reserved with InfixOperator
  case object PlusEquals                                      extends Operator("+=") with Reserved with InfixOperator
  case object MinusEquals                                     extends Operator("-=") with Reserved with InfixOperator
  case object LessThanOrEqual                                 extends Operator("<=") with Reserved with InfixOperator
  case object NotEqual                                        extends Operator("!=") with Reserved with InfixOperator
  case object ForwardArrow                                    extends Operator("->") with Reserved
  case object Or                                              extends Operator("||") with Reserved with InfixOperator
  case object And                                             extends Operator("&&") with Reserved with InfixOperator
  case object ShiftLeft                                       extends Operator("<<") with Reserved with InfixOperator
  case object ShiftRight                                      extends Operator(">>") with Reserved with InfixOperator
  case object ModuloAddition                                  extends Operator("|+|") with Reserved with InfixOperator
  case object ModuloSubtraction                               extends Operator("|-|") with Reserved with InfixOperator
  case object ModuloExponentiation                            extends Operator("|**|") with Reserved with InfixOperator
  case object ModuloMultiplication                            extends Operator("|*|") with Reserved with InfixOperator
  case object Exponentiation                                  extends Operator("**") with Reserved with InfixOperator
  case object Minus                                           extends Operator("-") with Reserved with InfixOperator with Unary
  case object Plus                                            extends Operator("+") with Reserved with InfixOperator with Unary
  case object Asterisk                                        extends Operator("*") with Reserved with InfixOperator
  case object ForwardSlash                                    extends Operator("/") with Reserved with InfixOperator
  case object GreaterThan                                     extends Operator(">") with Reserved with InfixOperator
  case object LessThan                                        extends Operator("<") with Reserved with InfixOperator
  case object Equal                                           extends Operator("=") with Reserved
  case object Exclamation                                     extends Operator("!") with Reserved with Unary
  case object Bar                                             extends Operator("|") with Reserved with InfixOperator
  case object Ampersand                                       extends Operator("&") with Reserved with InfixOperator
  case object Caret                                           extends Operator("^") with Reserved with InfixOperator
  case object Percent                                         extends Operator("%") with Reserved with InfixOperator

  sealed abstract class Delimiter(override val lexeme: String) extends Token
  case object OpenParen                                        extends Delimiter("(") with Reserved
  case object CloseParen                                       extends Delimiter(")") with Reserved
  case object OpenCurly                                        extends Delimiter("{") with Reserved
  case object CloseCurly                                       extends Delimiter("}") with Reserved
  case object OpenBracket                                      extends Delimiter("[") with Reserved
  case object BlockBracket                                     extends Delimiter("]") with Reserved
  case object Comma                                            extends Delimiter(",") with Reserved
  case object Dot                                              extends Delimiter(".") with Reserved
  case object Colon                                            extends Delimiter(":") with Reserved
  case object Semicolon                                        extends Delimiter(";") with Reserved
  case object DoubleForwardSlash                               extends Delimiter("//") with Reserved
  case object Newline                                          extends Delimiter(System.lineSeparator()) with Space
  case object Space                                            extends Delimiter(" ") with Space
  case object Tab                                              extends Delimiter("\t") with Space

  sealed abstract class Punctuator(override val lexeme: String) extends Token
  case object Hash                                              extends Punctuator("#") with Reserved
  case object Underscore                                        extends Punctuator("_")
  case object At                                                extends Punctuator("@") with Reserved
  case object Tick                                              extends Punctuator("`") with Reserved
  case object Quote                                             extends Punctuator("\"") with Reserved
  case object B                                                 extends Punctuator("b") with Token
  case object Dollar                                            extends Punctuator("$") with Reserved

  sealed abstract class Data(override val lexeme: String) extends Token
  case object Const                                       extends Data("const") with Reserved

  sealed abstract class DataDefinition(override val lexeme: String) extends Data(lexeme)
  case object Let                                                   extends DataDefinition("let") with Reserved
  case object Mut                                                   extends DataDefinition("mut") with Reserved
  case object Mapping                                               extends DataDefinition("mapping") with Reserved

  sealed abstract class DataTemplate(override val lexeme: String) extends Data(lexeme)
  case object Struct                                              extends DataTemplate("struct") with Reserved
  case object Enum                                                extends DataTemplate("enum") with Reserved
  case object Event                                               extends DataTemplate("event") with Reserved

  sealed abstract class Control(override val lexeme: String) extends Token
  case object If                                             extends Control("if") with Reserved
  case object Else                                           extends Control("else") with Reserved
  case object While                                          extends Control("while") with Reserved
  case object Return                                         extends Control("return") with Reserved
  case object For                                            extends Control("for") with Reserved

  sealed abstract class AccessModifier(override val lexeme: String) extends Token
  case object Pub                                                   extends AccessModifier("pub") with Reserved

  sealed abstract class Definition(override val lexeme: String) extends Token
  case object Fn                                                extends Definition("fn") with Reserved
  case object Import                                            extends Definition("import") with Reserved
  case object Abstract                                          extends Definition("Abstract") with Reserved

  sealed abstract class TemplateDefinition(override val lexeme: String) extends Definition(lexeme)
  case object Contract                                                  extends TemplateDefinition("Contract") with Reserved
  case object TxScript                                                  extends TemplateDefinition("TxScript") with Reserved
  case object AssetScript                                               extends TemplateDefinition("AssetScript") with Reserved
  case object Interface                                                 extends TemplateDefinition("Interface") with Reserved

  sealed abstract class Inheritance(override val lexeme: String) extends Token
  case object Extends                                            extends Inheritance("extends") with Reserved
  case object Implements                                         extends Inheritance("implements") with Reserved

  sealed abstract class Native(override val lexeme: String) extends Token
  case object Using                                         extends Native("using")
  case object Emit                                          extends Native("emit")
  case object Embeds                                        extends Native("embeds")

  sealed abstract class Primitive(override val lexeme: String) extends Token

  sealed abstract class PrimitiveBoolean(override val lexeme: String) extends Primitive(lexeme) with Expression
  case object True                                                    extends PrimitiveBoolean("true") with Reserved
  case object False                                                   extends PrimitiveBoolean("false") with Reserved

  sealed abstract class PrimitiveUnit(override val lexeme: String) extends Primitive(lexeme) with Expression
  case object AlphLowercase                                        extends PrimitiveUnit("alph") with Reserved
  case object AlphUppercase                                        extends PrimitiveUnit("ALPH") with Reserved

  val reserved: Array[Reserved] =
    EnumerationMacros
      .sealedInstancesOf[Reserved]
      .toArray
      .sorted

  val infix: Array[InfixOperator] =
    EnumerationMacros
      .sealedInstancesOf[InfixOperator]
      .toArray
      .sorted

  val spaces: List[Space] =
    EnumerationMacros
      .sealedInstancesOf[Space]
      .toList
      .sorted

  val inlineSpaces: List[Space] =
    spaces.filter(_ != Token.Newline)

  val unary: Array[Unary] =
    EnumerationMacros
      .sealedInstancesOf[Unary]
      .toArray
      .sorted

}
