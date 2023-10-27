package org.alephium.ralph.lsp.pc.completion

import org.alephium.ralph.lsp.pc.workspace.WorkspaceState

import java.io.File
import java.nio.file.{  Files, FileSystems, Paths}
import scala.io.Source
import scala.collection.JavaConverters._
import org.alephium.ralph.lsp.pc.util.PicklerUtil._

import scala.util.{Failure, Success, Using}
import java.net.URI
import scala.util.Random
import upickle.default._
import com.typesafe.scalalogging.StrictLogging

object CodeCompleter extends StrictLogging {

  /**
   * Execute code completing given the current workspace state.
   * Currently we ignore line/text context, we just return every built in functions
   */
  def complete(line: Int,
               character: Int,
               uri: URI,
               workspace: WorkspaceState.SourceAware ): Array[Suggestion] =
    workspace.build.dependencies.builtInFunctions.map(_.toSuggestion).toArray
}
