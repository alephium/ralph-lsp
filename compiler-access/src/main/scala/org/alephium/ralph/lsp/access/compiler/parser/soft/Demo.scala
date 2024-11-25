package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.CompilerAccess

object Demo extends App {

  val compiler =
    CompilerAccess.ralphc

  val ast =
    compiler.parseSoft {
      """
        |Contract HelloWorld(type: SomeType, tuple: (A, B)) {
        |
        |  fn function(nested_tuple: (A, (B, C))) -> ABC {
        |
        |  }
        |
        |  🚀
        |}
        |""".stripMargin
    } match {
      case Right(softAST) =>
        softAST

      case Left(parseError) =>
        // Format and print the FastParse error.
        // The goal here is to ensure that the parser always succeeds, regardless of the input.
        // Therefore, this error should never occur. If an error does occur, the parser should be updated to handle those cases.
        println(parseError.error.toFormatter().format(Some(Console.RED)))
        throw parseError.error
    }

  // Emit code generated from AST
  println("Parsed code:")
  println(ast.toCode())

  // Emit parsed AST
  println("SoftAST:")
  println(ast.toNode().toStringTree(_.toStringPretty()))

}
