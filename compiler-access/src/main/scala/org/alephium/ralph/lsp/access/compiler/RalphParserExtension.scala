package org.alephium.ralph.lsp.access.compiler

import fastparse._
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra._
import org.alephium.ralph.{Ast, StatefulParser, SourceIndex}

import java.net.URI

/** Functions that extend ralphc's default parser */
object RalphParserExtension {

  implicit val whitespace: P[_] => P[Unit] = new StatefulParser(None).whitespace

  /**
   * An extension to Ralphc's parse function [[org.alephium.ralph.StatefulParser.multiContract]]
   * that add support for import syntax.
   */
  def multiContract[Unknown: P](fileURI: URI): P[Tree.Root] =
    P(Start ~ Index ~ statement(fileURI).rep(1) ~ Index ~ End) map {
      case (fromIndex, statements, toIndex) =>
        val index =
          SourceIndex(
            index = fromIndex,
            width = toIndex - fromIndex,
            fileURI = Some(fileURI)
          )

        Tree.Root(
          statements = statements,
          index = index
        )
    }

  /** Parse an import identifier ignoring errors */
  def lazyParseImportIdentifier(identifier: String, fileURI: URI): Option[Tree.Import] =
    fastparse.parse(s"import \"$identifier\"", importStatement(fileURI)(_)) match {
      case Parsed.Success(tree, _) =>
        Some(tree)

      case _: Parsed.Failure =>
        None
    }

  /** A statement can be an import or ralphc's contract */
  private def statement[Unknown: P](fileURI: URI): P[Tree.Statement] =
    P(importStatement(fileURI) | sourceStatement(fileURI))

  /** Parse import syntax */
  private def importStatement[Unknown: P](fileURI: URI): P[Tree.Import] =
    P(Index ~~ "import" ~ stringLiteral(fileURI) ~~ Index) map {
      case (fromIndex, stringLiteral, toIndex) =>
        val importIndex =
          SourceIndex(
            index = fromIndex,
            width = toIndex - fromIndex,
            fileURI = Some(fileURI)
          )

        val importPath =
          parsePath(
            name = stringLiteral.name,
            fileURI = fileURI
          )

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
  private def parsePath(name: Tree.Name, fileURI: URI): Option[Tree.ImportPath] =
    fastparse.parse(name.value, importPaths(fileURI)(_)) match {
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

  private def importPaths[Unknown: P](fileURI: URI): P[(Tree.Name, Tree.Name)] =
    // Parser for `std/nft_interface`
    P(Index ~~ CharsWhile(_ != '/').! ~~ Index ~ "/" ~~ Index ~~ AnyChar.rep.! ~~ Index) map {
      case (fromPackageIndex, packageName, toPackageIndex, fromFileNameIndex, fileName, toFileNameIndex) =>
        val packageIndex =
          SourceIndex(
            index = fromPackageIndex,
            width = toPackageIndex - fromPackageIndex,
            fileURI = Some(fileURI)
          )

        val fileIndex =
          SourceIndex(
            index = fromFileNameIndex,
            width = toFileNameIndex - fromFileNameIndex,
            fileURI = Some(fileURI)
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
   * Parse a ralphc contract.
   *
   * This function is a clone of [[org.alephium.ralph.StatefulParser.multiContract]]
   * but without the requirement that it be the start of the file, so imports are allowed.
   */
  private def sourceStatement[Unknown: P](fileURI: URI): P[Tree.Source] = {
    val ralphParser =
      new StatefulParser(Some(fileURI))

    P(
      Index ~~ (ralphParser.rawTxScript | ralphParser.rawContract | ralphParser.rawInterface | ralphParser.rawStruct) ~~ Index
    ) map {
      case (fromIndex, code, toIndex) =>
        val ast =
          code match {
            case struct: Ast.Struct =>
              Right(struct)

            case contract: Ast.ContractWithState =>
              Left(contract)

            case _: Ast.AssetScript =>
              // parser functions from ralphc also result in the `Ast.AssetScript` type.
              // For whatever reason, if `AssetScript` is returned, report it as a parser failure since it is unexpected.
              val ctx = implicitly[P[_]]
              val fail = ctx.freshFailure()
              ctx.setMsg(fromIndex, () => "TxScript, Contract, Interface or Struct. Found AssetScript.")
              return fail
          }

        val index =
          SourceIndex(
            index = fromIndex,
            width = toIndex - fromIndex,
            fileURI = Some(fileURI)
          )

        Tree.Source(
          ast = ast,
          index = index
        )
    }
  }

  /**
   * A string literal. For example in the following code
   * `"package_name/file"` is a string literal.
   *
   * {{{
   *   import "package_name/file"
   * }}}
   */
  private def stringLiteral[Unknown: P](fileURI: URI): P[Tree.StringLiteral] =
    P(Index ~~ "\"" ~~ Index ~~ CharsWhile(_ != '"').!.? ~~ Index ~~ "\"" ~~ Index) map { // TODO: See if negative look ahead with AnyChar.rep would work instead of `CharsWhile`
      case (fromIndex, nameFromIndex, string, nameToIndex, toIndex) =>
        val index =
          SourceIndex(
            index = fromIndex,
            width = toIndex - fromIndex,
            fileURI = Some(fileURI)
          )

        val value =
          string getOrElse ""

        val name =
          Tree.Name(
            value = value,
            index = SourceIndex(
              index = nameFromIndex,
              width = nameToIndex - nameFromIndex,
              fileURI = Some(fileURI)
            )
          )

        Tree.StringLiteral(
          value = s"\"$value\"",
          name = name,
          index = index
        )
    }
}
