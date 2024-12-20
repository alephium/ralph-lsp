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

import org.alephium.macros.EnumerationMacros

sealed trait Token { self =>

  def lexeme: String

}

object Token {

  /**
   * Represents tokens that are specifically reserved for use in Ralph's grammar.
   *
   * These tokens cannot be used as identifier [[SoftAST.Identifier]].
   */
  sealed trait Reserved extends Token

  sealed abstract class Operator(override val lexeme: String) extends Token
  case object Equality                                        extends Operator("==") with Reserved
  case object GreaterThanOrEqual                              extends Operator(">=") with Reserved
  case object PlusEquals                                      extends Operator("+=") with Reserved
  case object MinusEquals                                     extends Operator("-=") with Reserved
  case object LessThanOrEqual                                 extends Operator("<=") with Reserved
  case object NotEqual                                        extends Operator("!=") with Reserved
  case object ForwardArrow                                    extends Operator("->") with Reserved
  case object Or                                              extends Operator("||") with Reserved
  case object And                                             extends Operator("&&") with Reserved
  case object Minus                                           extends Operator("-") with Reserved
  case object Plus                                            extends Operator("+") with Reserved
  case object Asterisk                                        extends Operator("*") with Reserved
  case object ForwardSlash                                    extends Operator("/") with Reserved
  case object GreaterThan                                     extends Operator(">") with Reserved
  case object LessThan                                        extends Operator("<") with Reserved
  case object Equal                                           extends Operator("=") with Reserved
  case object Exclamation                                     extends Operator("!") with Reserved
  case object Bar                                             extends Operator("|") with Reserved
  case object Ampersand                                       extends Operator("&") with Reserved
  case object Caret                                           extends Operator("^") with Reserved
  case object Percent                                         extends Operator("%") with Reserved

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
  case object Newline                                          extends Delimiter(System.lineSeparator())
  case object Space                                            extends Delimiter(" ")
  case object DoubleForwardSlash                               extends Delimiter("//") with Reserved

  sealed abstract class Punctuator(override val lexeme: String) extends Token
  case object Hash                                              extends Punctuator("#") with Reserved
  case object Underscore                                        extends Punctuator("_")
  case object At                                                extends Punctuator("@") with Reserved
  case object Tick                                              extends Punctuator("`") with Reserved
  case object Quote                                             extends Punctuator("\"") with Reserved

  sealed abstract class Data(override val lexeme: String) extends Token
  case object Let                                         extends Data("let") with Reserved
  case object Mut                                         extends Data("mut") with Reserved
  case object Struct                                      extends Data("struct") with Reserved
  case object Const                                       extends Data("const") with Reserved
  case object Enum                                        extends Data("enum") with Reserved
  case object Event                                       extends Data("event") with Reserved

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
  case object Contract                                          extends Definition("Contract") with Reserved
  case object Abstract                                          extends Definition("Abstract") with Reserved
  case object TxScript                                          extends Definition("TxScript") with Reserved
  case object AssetScript                                       extends Definition("AssetScript") with Reserved
  case object Interface                                         extends Definition("Interface") with Reserved

  sealed abstract class Inheritance(override val lexeme: String) extends Token
  case object Extends                                            extends Inheritance("extends") with Reserved
  case object Implements                                         extends Inheritance("implements") with Reserved

  sealed abstract class Native(override val lexeme: String) extends Token
  case object Using                                         extends Native("using")
  case object Emit                                          extends Native("emit")
  case object Embeds                                        extends Native("embeds")

  sealed abstract class Primitive(override val lexeme: String) extends Token
  case object True                                             extends Primitive("true") with Reserved
  case object False                                            extends Primitive("false") with Reserved
  case object Alph_Small                                       extends Primitive("alph") with Reserved
  case object Alph_Big                                         extends Primitive("ALPH") with Reserved

  sealed trait Term                        extends Token
  case class Name(lexeme: String)          extends Term
  case class NumberLiteral(lexeme: String) extends Term
  case class StringLiteral(lexeme: String) extends Term

  /**
   * Order such that tokens such as:
   *  - `||` come before `|`
   *  - `+=` come before `+` and `=`
   */
  implicit val reservedDescendingOrdering: Ordering[Reserved] = {
    case (x: Reserved, y: Reserved) =>
      val lengthCompare =
        y.lexeme.length compare x.lexeme.length

      if (lengthCompare == 0)
        y.lexeme compare x.lexeme
      else
        lengthCompare
  }

  val reserved: Array[Reserved] =
    EnumerationMacros
      .sealedInstancesOf[Reserved]
      .toArray
      .sorted

}
