package org.alephium.ralph.lsp.pc.completion

import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

import java.net.URI
import scala.util.Random

object CodeCompleter {

  /**
   * Execute code completing given the current workspace state.
   */
  def complete(line: Int,
               character: Int,
               uri: URI,
               workspace: WorkspaceState): Array[Suggestion] = {
    val randomFunctionName = List("deposit", "transfer", "allocate", "upgrade", "assert")
    val randomParamName = List("id", "name", "address", "addr")
    val inputTypes = List("U256", "Address", "Bool")
    val returnTypes = inputTypes ++ List("()")

    def dummySuggestion() = {
      val functionName = Random.shuffle(randomFunctionName).head
      val paramName = Random.shuffle(randomParamName).head
      val typeName = Random.shuffle(inputTypes).head
      val returnType = Random.shuffle(returnTypes).head

      Suggestion.Function(
        label = s"$functionName($paramName: $typeName) -> $returnType",
        insert = s"$functionName(???)",
        detail = s"$functionName detailed",
        documentation = s"documentation for $functionName"
      )
    }

    Array.fill(Random.nextInt(10) max 3)(dummySuggestion())
  }

}
