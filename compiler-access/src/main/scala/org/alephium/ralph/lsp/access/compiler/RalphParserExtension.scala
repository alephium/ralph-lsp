package org.alephium.ralph.lsp.access.compiler

import fastparse._
import org.alephium.ralph.StatefulParser.{rawContract, rawInterface, rawTxScript, whitespace}
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.SourceIndex

/** Functions that extend ralphc's default parser */
object RalphParserExtension {

  /**
   * An extension to Ralphc's parse function [[org.alephium.ralph.StatefulParser.multiContract]]
   * that add support for import syntax.
   */
  def multiContract[Unknown: P]: P[Tree.Root] =
    P(Start ~ Index ~ statement.rep(1) ~ Index ~ End) map {
      case (fromIndex, statements, toIndex) =>
        val index =
          SourceIndex(
            from = fromIndex,
            width = toIndex - fromIndex
          )

        Tree.Root(
          statements = statements,
          index = index
        )
    }

  /** Parse an import identifier ignoring errors */
  def lazyParseImportIdentifier(identifier: String): Option[Tree.Import] =
    fastparse.parse(s"import \"$identifier\"", importStatement(_)) match {
      case Parsed.Success(tree, _) =>
        Some(tree)

      case _: Parsed.Failure =>
        None
    }

  /** A statement can be an import or ralphc's contract */
  private def statement[Unknown: P]: P[Tree.Statement] =
    P(importStatement | sourceStatement)

  /** Parse import syntax */
  private def importStatement[Unknown: P]: P[Tree.Import] =
    P(Index ~~ "import" ~ stringLiteral ~~ Index) map {
      case (fromIndex, stringLiteral, toIndex) =>
        val importIndex =
          SourceIndex(
            from = fromIndex,
            width = toIndex - fromIndex
          )

        val importPath =
          parsePath(stringLiteral.name)

        Tree.Import(
          string = stringLiteral,
          path = importPath,
          index = importIndex
        )
    }

  /**
   * Lazily parse the string literal to separate the folder-name and file-name.
   *
   * On error, ignore parse.
   *
   * @param name The String value to parse.
   */
  private def parsePath(name: Tree.Name): Option[Tree.ImportPath] =
    fastparse.parse(name.value, importPaths(_)) match {
      case Parsed.Success((packagePath, filePath), _) =>
        // the above parse occurs on a string value, add the offset such
        // that the indexes are set according to the entire source-code.
        val offsetIndex =
          name.index.from

        val path =
          Tree.ImportPath(
            folder = packagePath.copy(index = packagePath.index + offsetIndex),
            file = filePath.copy(index = filePath.index + offsetIndex),
            index = name.index
          )

        Some(path)

      case _: Parsed.Failure =>
        None
    }

  private def importPaths[Unknown: P]: P[(Tree.Name, Tree.Name)] =
    // Parser for `std/nft_interface`
    P(Index ~~ CharsWhile(_ != '/').! ~~ Index ~ "/" ~~ Index ~~ AnyChar.rep.! ~~ Index) map {
      case (fromPackageIndex, packageName, toPackageIndex, fromFileNameIndex, fileName, toFileNameIndex) =>
        val packageIndex =
          SourceIndex(
            from = fromPackageIndex,
            width = toPackageIndex - fromPackageIndex
          )

        val fileIndex =
          SourceIndex(
            from = fromFileNameIndex,
            width = toFileNameIndex - fromFileNameIndex
          )

        val packagePath =
          Tree.Name(
            value = packageName,
            index = packageIndex
          )

        val filePath =
          Tree.Name(
            value = fileName,
            index = fileIndex
          )

        (packagePath, filePath)
    }


  /**
   *
   * Parse a ralphc contract.
   *
   * This function is a clone of [[org.alephium.ralph.StatefulParser.multiContract]]
   * but without the requirement that it be the start of the file, so imports are allowed.
   * */
  private def sourceStatement[Unknown: P]: P[Tree.Source] =
    P(Index ~~ (rawTxScript | rawContract | rawInterface) ~~ Index) map {
      case (fromIndex, code, toIndex) =>
        val index =
          SourceIndex(
            from = fromIndex,
            width = toIndex - fromIndex
          )

        Tree.Source(
          ast = code,
          index = index
        )
    }

  /**
   * A string literal. For example in the following code
   * `"package_name/file"` is a string literal.
   *
   * {{{
   *   import "package_name/file"
   * }}}
   * */
  private def stringLiteral[Unknown: P]: P[Tree.StringLiteral] =
    P(Index ~~ "\"" ~~ Index ~~ CharsWhile(_ != '"').!.? ~~ Index ~~ "\"" ~~ Index) map { // TODO: See if negative look ahead with AnyChar.rep would work instead of `CharsWhile`
      case (fromIndex, nameFromIndex, string, nameToIndex, toIndex) =>
        val index =
          SourceIndex(
            from = fromIndex,
            width = toIndex - fromIndex
          )

        val value =
          string getOrElse ""

        val name =
          Tree.Name(
            value = value,
            index = SourceIndex(
              from = nameFromIndex,
              width = nameToIndex - nameFromIndex
            )
          )

        Tree.StringLiteral(
          value = s"\"$value\"",
          name = name,
          index = index
        )
    }
}
