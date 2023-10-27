package org.alephium.ralph.lsp.pc.workspace.build

import org.alephium.ralph.lsp.pc.completion.BuiltInFunction

case class BuildDependencies(
  stdInterfaces: Map[String, String],
  builtInFunctions: Seq[BuiltInFunction]
)

object BuildDependencies {
  def empty: BuildDependencies =
    BuildDependencies(
      stdInterfaces = Map.empty,
      builtInFunctions = Seq.empty
    )
}
