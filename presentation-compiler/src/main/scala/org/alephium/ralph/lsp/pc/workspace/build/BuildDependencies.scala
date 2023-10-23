package org.alephium.ralph.lsp.pc.workspace.build

case class BuildDependencies(
  stdInterfaces: Map[String, String]
)

object BuildDependencies {
  def empty: BuildDependencies =
    BuildDependencies(
      stdInterfaces = Map.empty
    )
}