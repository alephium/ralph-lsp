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

package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.Ast
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.LineRange
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension

/** Represents a position within a source-file in parsed state. */
sealed trait SourceLocation {

  def parsed: SourceCodeState.Parsed

}

object SourceLocation {

  /**
   * Result types for GoTo location search results.
   */
  sealed trait GoTo extends SourceLocation {

    def toLineRange(): Option[LineRange]

  }

  /**
   * Result types for GoTo definition location search results.
   */
  sealed trait GoToDef extends GoTo

  /**
   * Result types for renaming location search results.
   */
  sealed trait GoToRename extends GoTo

  /**
   * Result types for GoTo references location search results.
   */
  sealed trait GoToRef extends GoTo

  /**
   * Represents a source file ([[SourceCodeState.Parsed]]) without
   * a target position. For eg: Used to provide jump definition for imported files.
   *
   * @param parsed The source file containing the positioned node.
   */
  case class File(parsed: SourceCodeState.Parsed) extends GoToDef {

    def lineRange(): LineRange =
      LineRange.zero

    override def toLineRange(): Option[LineRange] =
      Some(lineRange())

  }

  /**
   * Represents a position with an import statement.
   *
   * @param name   The name of the file or folder.
   * @param parsed The parsed source source-code containing the import statement.
   */
  case class ImportName(
      name: Tree.Name,
      parsed: SourceCodeState.Parsed)
    extends GoToRef
       with GoToRename {

    def lineRange(): LineRange =
      name.index.toLineRange(parsed.code)

    override def toLineRange(): Option[LineRange] =
      Some(lineRange())

  }

  /**
   * Represents a single positioned AST ([[org.alephium.ralph.Ast.Positioned]])
   * within a source tree ([[SourceLocation.Code]]),
   *
   * @param ast    The positioned node within the parsed source file.
   * @param source The source tree containing the positioned node.
   */
  case class Node[+A <: Ast.Positioned](
      ast: A,
      source: SourceLocation.Code)
    extends GoToDef
       with GoToRef
       with GoToRename {

    def toLineRange(): Option[LineRange] =
      ast
        .sourceIndex
        .map(_.toLineRange(source.parsed.code))

    override def parsed: SourceCodeState.Parsed =
      source.parsed

  }

  /**
   * Represents a single source tree ([[Tree.Source]]) within a source file ([[SourceCodeState.Parsed]]),
   * which can contain multiple source trees such as contracts, scripts etc.
   *
   * @param tree   The source tree within the parsed source file.
   * @param parsed The source file containing the source tree.
   */
  case class Code(
      tree: Tree.Source,
      parsed: SourceCodeState.Parsed)
    extends SourceLocation

}
