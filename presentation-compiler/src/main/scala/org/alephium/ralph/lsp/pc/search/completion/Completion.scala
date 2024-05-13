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

package org.alephium.ralph.lsp.pc.search.completion

/**
 * Represents string based code completion types recognised by LSP:
 * <a href="https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItemKind">CompletionItemKind</a>
 *
 * These types hold string values. Use [[Suggestion]] instead for typed code-completion that is native to presentation-compiler.
 */
sealed trait Completion extends Product {

  /** The label or display text for the suggestion. */
  def label: String

  /** The text to insert when the suggestion is selected. */
  def insert: String

  /** Additional details about the suggestion. */
  def detail: String

}

/**
 * These types were copied from the class `org.eclipse.lsp4j.CompletionItemKind`.
 *
 * @see <a href="https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#completionItemKind">CompletionItemKind</a>
 */
object Completion {

  case class Text(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class Method(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class Function(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class Constructor(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class Field(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class Variable(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class Class(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class Interface(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class Module(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class Property(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class Unit(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class Value(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class Enum(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class Keyword(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class Snippet(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class Color(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class File(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class Reference(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class Folder(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class EnumMember(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class Constant(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class Struct(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class Event(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class Operator(
      label: String,
      insert: String,
      detail: String)
    extends Completion

  case class TypeParameter(
      label: String,
      insert: String,
      detail: String)
    extends Completion

}
