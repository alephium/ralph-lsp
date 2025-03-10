// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

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
