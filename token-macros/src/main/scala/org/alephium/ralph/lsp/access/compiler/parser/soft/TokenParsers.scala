// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import fastparse.P
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.Token

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/** Expands token collections into a static FastParse trie and typed singleton dispatch. */
object TokenParsers {

  def parse[T <: Token](
      tokens: Seq[T],
      prefixCheck: Boolean
    )(implicit ctx: P[_]): P[T] =
    macro TokenParserMacros.parse[T]

}

object TokenParserMacros {

  def parse[T <: Token: c.WeakTypeTag](
      c: blackbox.Context
    )(tokens: c.Expr[Seq[T]],
      prefixCheck: c.Expr[Boolean]
    )(ctx: c.Expr[P[_]]): c.Expr[P[T]] = {
    import c.universe._
    val values =
      try c.eval(c.Expr[Seq[Token]](c.untypecheck(tokens.tree.duplicate))).sorted
      catch { case error: Exception => c.abort(tokens.tree.pos, s"Tokens must be available at compile time: ${error.getMessage}") }
    val check = prefixCheck.tree match {
      case Literal(Constant(value: Boolean)) => value
      case _                                 => c.abort(prefixCheck.tree.pos, "prefixCheck must be a Boolean literal")
    }
    if (values.isEmpty) c.abort(tokens.tree.pos, "At least one token is required")
    if (values.map(_.lexeme).distinct.size != values.size) c.abort(tokens.tree.pos, "Token lexemes must be unique")
    if (values.exists(_.lexeme.isEmpty)) c.abort(tokens.tree.pos, "Token lexemes must be nonempty")
    val resultType = weakTypeOf[T]
    def parserFor(newline: String): Tree = {
      val entries = values
        .map(
          token => (if (token == Token.Newline) newline else token.lexeme, token)
        )
        .sortWith {
          case ((left, _), (right, _)) => left.length > right.length || (left.length == right.length && left > right)
        }
      val cases = entries.map {
        case (lexeme, token) =>
          val reference = c.parse("_root_." + token.getClass.getName.stripSuffix("$").replace('$', '.'))
          cq"$lexeme => ($reference: $resultType)"
      }
      val default  = cq"""_ => throw new _root_.java.lang.IllegalStateException("Unrecognized statically parsed token")"""
      val dispatch = cases :+ default
      val strings = entries.map {
        case (lexeme, _) => Literal(Constant(lexeme))
      }
      if (!check) {
        val literals = TermName(c.freshName("tokenLiterals"))
        val value    = TermName(c.freshName("tokenValue"))
        // Keep the literal trie and dispatch separate under the repository's whole-program inliner.
        q"""{
          @_root_.scala.noinline def $literals(): _root_.fastparse.P[Unit] = _root_.fastparse.StringIn(..$strings)($ctx)
          @_root_.scala.noinline def $value(text: String): $resultType = text match { case ..$dispatch }
          $literals().!.map($value)
        }"""
      } else {
        val alternatives = entries.map {
          case (lexeme, token) =>
            val reference = c.parse("_root_." + token.getClass.getName.stripSuffix("$").replace('$', '.'))
            val excluded  = token.otherReservedTokensWithThisPrefix.map(_.lexeme)
            val literal   = q"_root_.fastparse.StringIn($lexeme)($ctx)"
            val guarded   = if (excluded.isEmpty) literal else q"!_root_.fastparse.StringIn(..$excluded)($ctx) ~ $literal"
            q"($guarded).map(_ => ($reference: $resultType))"
        }
        alternatives.reduceLeft(
          (left,
              right) => q"$left | $right"
        )
      }
    }
    // A JAR compiled on Linux must still follow Token.Newline on Windows.
    val guarded =
      if (values.contains(Token.Newline))
        q"if (_root_.java.lang.System.lineSeparator() == ${"\r\n"}) ${parserFor("\r\n")} else ${parserFor("\n")}"
      else parserFor("\n")
    val entry = TermName(c.freshName("parseToken"))
    c.Expr[P[T]](q"""{
      import _root_.fastparse._
      import _root_.fastparse.NoWhitespace.noWhitespaceImplicit
      @_root_.scala.noinline def $entry(): _root_.fastparse.P[$resultType] = $guarded
      $entry()
    }""")
  }

}
