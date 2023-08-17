package org.alephium.ralph.lsp.pc.completion

import org.alephium.ralph.Ast
import org.eclipse.lsp4j._

import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters.SeqHasAsJava
import scala.util.Random

object CodeCompleter {

  // TODO: Build semantic index for code-completion using this AST (`SourcePosition`s in AST are required)
  def buildSemanticIndex(uri: String, ast: Ast.AssetScript): Future[Unit] =
    Future.unit

  /**
   * Dummy completion.
   */
  def complete(position: CompletionParams)(implicit ec: ExecutionContext): Future[CompletionList] =
    Future {
      val list = new CompletionList()

      // Dummy data
      val randomFunctionName = List("deposit", "transfer", "allocate", "upgrade", "assert")
      val randomParamName = List("id", "name", "address", "addr")
      val inputTypes = List("U256", "Address", "Bool")
      val returnTypes = inputTypes ++ List("()")

      def generateDummyCompletion() = {
        val functionName = Random.shuffle(randomFunctionName).head
        val paramName = Random.shuffle(randomParamName).head
        val typeName = Random.shuffle(inputTypes).head
        val returnType = Random.shuffle(returnTypes).head

        val item = new CompletionItem()
        item.setLabel(s"$functionName($paramName: $typeName) -> $returnType")
        item.setDetail(s"$functionName detailed")
        item.setDocumentation(s"documentation for $functionName")
        item.setInsertText(s"$functionName(???)")
        item.setKind(CompletionItemKind.Function)
        item
      }

      // randomly generate completing items
      val items =
        (0 to (Random.nextInt(10) max 3)) map {
          _ =>
            generateDummyCompletion()
        }

      list.setItems(items.asJava)

      list
    }
}
