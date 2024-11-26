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

sealed trait Token { self =>

  def lexeme: String

}

object Token {

  sealed abstract class Operator(override val lexeme: String) extends Token
  case object Negative                                        extends Operator("-")
  case object Positive                                        extends Operator("+")
  case object Plus                                            extends Operator("+")
  case object Minus                                           extends Operator("-")
  case object Asterisk                                        extends Operator("*")
  case object ForwardSlash                                    extends Operator("/")
  case object DoubleForwardSlash                              extends Operator("//")
  case object GreaterThan                                     extends Operator(">")
  case object LessThan                                        extends Operator("<")
  case object GreaterThanOrEqual                              extends Operator(">=")
  case object LessThanOrEqual                                 extends Operator("<=")
  case object Equal                                           extends Operator("=")
  case object NotEqual                                        extends Operator("!=")
  case object Exclamation                                     extends Operator("!")
  case object Bar                                             extends Operator("|")
  case object Ampersand                                       extends Operator("&")
  case object Caret                                           extends Operator("^")
  case object Percent                                         extends Operator("%")
  case object ForwardArrow                                    extends Operator("->")
  case object Or                                              extends Operator("||")
  case object And                                             extends Operator("&&")

  sealed abstract class Delimiter(override val lexeme: String) extends Token
  case object OpenParen                                        extends Delimiter("(")
  case object CloseParen                                       extends Delimiter(")")
  case object OpenCurly                                        extends Delimiter("{")
  case object CloseCurly                                       extends Delimiter("}")
  case object OpenBracket                                      extends Delimiter("[")
  case object BlockBracket                                     extends Delimiter("]")
  case object Comma                                            extends Delimiter(",")
  case object Dot                                              extends Delimiter(".")
  case object Colon                                            extends Delimiter(":")
  case object Semicolon                                        extends Delimiter(";")
  case object Newline                                          extends Delimiter(System.lineSeparator())
  case object Space                                            extends Delimiter(" ")

  sealed abstract class Punctuator(override val lexeme: String) extends Token
  case object Hash                                              extends Punctuator("#")
  case object Underscore                                        extends Punctuator("_")
  case object At                                                extends Punctuator("@")
  case object Tick                                              extends Punctuator("`")
  case object Quote                                             extends Punctuator("\"")

  sealed abstract class Data(override val lexeme: String) extends Token
  case object Let                                         extends Data("let")
  case object Mut                                         extends Data("mut")
  case object Struct                                      extends Data("struct")
  case object Const                                       extends Data("const")
  case object Enum                                        extends Data("enum")

  sealed abstract class Control(override val lexeme: String) extends Token
  case object If                                             extends Control("if")
  case object Else                                           extends Control("else")
  case object While                                          extends Control("while")
  case object Return                                         extends Control("return")
  case object For                                            extends Control("for")

  sealed abstract class AccessModifier(override val lexeme: String) extends Token
  case object Pub                                                   extends AccessModifier("pub")

  sealed abstract class Definition(override val lexeme: String) extends Token
  case object Fn                                                extends Definition("fn")
  case object Event                                             extends Definition("event")
  case object Import                                            extends Definition("import")
  case object Contract                                          extends Definition("Contract")
  case object Abstract                                          extends Definition("Abstract")
  case object TxScript                                          extends Definition("TxScript")
  case object AssetScript                                       extends Definition("AssetScript")
  case object Interface                                         extends Definition("Interface")

  sealed abstract class Inheritance(override val lexeme: String) extends Token
  case object Extends                                            extends Inheritance("extends")
  case object Implements                                         extends Inheritance("implements")

  sealed abstract class Native(override val lexeme: String) extends Token
  case object Using                                         extends Native("using")
  case object Emit                                          extends Native("emit")
  case object Embeds                                        extends Native("embeds")

  sealed abstract class Primitive(override val lexeme: String) extends Token
  case object True                                             extends Primitive("true")
  case object False                                            extends Primitive("false")
  case object Alph_Small                                       extends Primitive("alph")
  case object Alph_Big                                         extends Primitive("ALPH")

  sealed trait Term                        extends Token
  case class Name(lexeme: String)          extends Term
  case class NumberLiteral(lexeme: String) extends Term
  case class StringLiteral(lexeme: String) extends Term

}
