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

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.pc.sourcecode.SourceLocation

private object IncludeDeclaration {

  def add(
      definitionAST: Ast.TypeId,
      definitionSource: SourceLocation.CodeStrict,
      result: Iterator[SourceLocation.NodeStrict[Ast.Positioned]],
      isIncludeDeclaration: Boolean): Iterator[SourceLocation.NodeStrict[Ast.Positioned]] =
    __addIDDefinition(
      definitionAST = definitionAST,
      definitionSource = definitionSource,
      result = result,
      isIncludeDeclaration = isIncludeDeclaration
    )

  def add(
      definitionAST: Ast.Ident,
      definitionSource: SourceLocation.CodeStrict,
      result: Iterator[SourceLocation.NodeStrict[Ast.Positioned]],
      isIncludeDeclaration: Boolean): Iterator[SourceLocation.NodeStrict[Ast.Positioned]] =
    __addIDDefinition(
      definitionAST = definitionAST,
      definitionSource = definitionSource,
      result = result,
      isIncludeDeclaration = isIncludeDeclaration
    )

  def add(
      definitionAST: Ast.FuncId,
      definitionSource: SourceLocation.CodeStrict,
      result: Iterator[SourceLocation.NodeStrict[Ast.Positioned]],
      isIncludeDeclaration: Boolean): Iterator[SourceLocation.NodeStrict[Ast.Positioned]] =
    __addIDDefinition(
      definitionAST = definitionAST,
      definitionSource = definitionSource,
      result = result,
      isIncludeDeclaration = isIncludeDeclaration
    )

  /**
   * DO NOT CALL THIS FUNCTION DIRECTLY. Use one of the above instead.
   *
   * This function is needed because the AST ID types ([[Ast.TypeId]], [[Ast.Ident]], [[Ast.FuncId]])
   * do not have a common subtype. And only these three types should be added to the result when
   * `isIncludeDeclaration` is `true`.
   */
  @inline private def __addIDDefinition[A <: Ast.Positioned](
      definitionAST: A,
      definitionSource: SourceLocation.CodeStrict,
      result: Iterator[SourceLocation.NodeStrict[Ast.Positioned]],
      isIncludeDeclaration: Boolean): Iterator[SourceLocation.NodeStrict[Ast.Positioned]] =
    if (isIncludeDeclaration) {
      val definition =
        SourceLocation.NodeStrict(
          ast = definitionAST,
          source = definitionSource
        )

      Iterator.single(definition) ++ result
    } else {
      result
    }

}
