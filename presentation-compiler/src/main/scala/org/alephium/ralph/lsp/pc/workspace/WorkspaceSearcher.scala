package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.pc.sourcecode.{SourceTreeInScope, SourceCodeState, SourceCodeSearcher}
import org.alephium.ralph.lsp.pc.workspace.build.dependency.DependencyID

import scala.collection.immutable.ArraySeq

/** Implements search functions that run on [[WorkspaceState.IsSourceAware]] */
object WorkspaceSearcher {

  /**
   * Collects all source trees within the scope of the provided source code.
   *
   * @param sourceCode The source code for which in-scope files are being searched.
   * @param workspace  The workspace that may contain files within the scope.
   * @return The source trees within the scope.
   */
  def collectInScope(
      sourceCode: SourceTreeInScope,
      workspace: WorkspaceState.IsSourceAware): Seq[SourceTreeInScope] = {
    val allInScopeCode =
      collectParsedInScope(workspace)

    val inheritancesInScope =
      SourceCodeSearcher.collectInheritanceInScope(
        source = sourceCode.tree,
        allSource = allInScopeCode
      )

    inheritancesInScope :+ sourceCode
  }

  /**
   * Collects all parsed source files, excluding `std` dependency source files
   * that are not imported.
   *
   * @param workspace The workspace with dependencies.
   * @return Parsed source files in scope.
   */
  private def collectParsedInScope(workspace: WorkspaceState.IsSourceAware): ArraySeq[SourceCodeState.Parsed] = {
    // fetch the `std` dependency
    val stdSourceParsedCode =
      workspace
        .build
        .findDependency(DependencyID.Std)
        .toSeq
        .flatMap(_.sourceCode.map(_.parsed))

    // collect all parsed source-files
    val workspaceCode =
      SourceCodeSearcher.collectParsed(workspace.sourceCode)

    // collect all import statements
    val importStatements =
      SourceCodeSearcher
        .collectImportStatements(workspaceCode)
        .map(_.string.value)

    // filter out std files that are not imported
    val importedCode =
      stdSourceParsedCode filter {
        stdCode =>
          stdCode
            .importIdentifier
            .exists {
              stdImportIdentifier =>
                importStatements contains stdImportIdentifier.string.value
            }
      }

    workspaceCode ++ importedCode
  }

}
