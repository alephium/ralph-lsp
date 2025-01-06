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

import org.alephium.ralph.{Type, Parser, Keyword}

object AnnotationCompleter {

  /**
   * Suggests the `@using` annotation name.
   *
   * According to the <a href="https://docs.alephium.org/dapps/ralph/getting-started/#annotations">Annotations</a>
   * documentation, only the `@using` annotation is supported.
   *
   * @return An iterator over the supported annotations.
   */
  def suggestAnnotationNames(): Iterator[Suggestion.Keyword] =
    Iterator(
      Suggestion.Keyword(
        label = Parser.FunctionUsingAnnotation.id,
        insert = Parser.FunctionUsingAnnotation.id,
        detail = ""
      ),
      Suggestion.Keyword(
        label = Parser.FunctionInlineAnnotation.id,
        insert = Parser.FunctionInlineAnnotation.id,
        detail = ""
      )
    )

  /**
   * Suggests `@using` annotation keys.
   *
   * According to the <a href="https://docs.alephium.org/dapps/ralph/getting-started/#annotations">Annotations</a>
   * documentation, only the `@using` annotation is supported. Therefore, this method only suggests fields for
   * `using` annotations.
   *
   * @return An iterator over `@using` annotation fields.
   * @note Currently hardcoded to support only [[Parser.FunctionUsingAnnotation.keys]],
   *       as only the `@using` annotation is supported.
   */
  def suggestAnnotationKeys(): Iterator[Suggestion.AnnotationField] =
    Parser.FunctionUsingAnnotation.keys.iterator.map {
      key =>
        Suggestion.AnnotationField(
          label = key,
          insert = key,
          detail = Type.Bool.signature
        )
    }

  /**
   * Suggests `@using` annotation values.
   *
   * @return An iterator over `@using` annotation values.
   * @note Currently hardcoded to support only the values of [[Parser.FunctionUsingAnnotation.keys]],
   *       as only the `@using` annotation is supported.
   */
  def suggestAnnotationValues(): Iterator[Suggestion.Keyword] =
    Iterator(
      Suggestion.Keyword.value(Keyword.`true`),
      Suggestion.Keyword.value(Keyword.`false`)
    )

}
