// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndexExtra}
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.sourcecode.imports.Importer
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.alephium.ralph.lsp.utils.CollectionUtil._
import org.alephium.ralph.lsp.utils.URIUtil

import java.net.URI
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

/**
 * Implements functions operating on source-code within a single file.
 */
private[pc] object SourceCode {

  /** Fetch all source files on disk */
  def initialise(sourceDirectory: URI)(implicit file: FileAccess): Either[CompilerMessage.AnyError, ArraySeq[SourceCodeState.OnDisk]] =
    file
      .list(sourceDirectory)
      .map(_.map(SourceCodeState.OnDisk(_)).to(ArraySeq))

  /**
   * Synchronise source files with files on disk.
   *
   * When a workspace file structure is moved or renamed,
   * then in certain situations, the known source-files in memory are lost.
   * This ensures that all files on-disk are still known to the workspace.
   *
   * @param sourceDirectory Directory to synchronise with
   * @param sourceCode      Collection to add missing source files
   * @return Source files that are in-sync with files on disk.
   */
  def synchronise(
      sourceDirectory: URI,
      sourceCode: ArraySeq[SourceCodeState]
    )(implicit file: FileAccess): Either[CompilerMessage.AnyError, ArraySeq[SourceCodeState]] =
    initialise(sourceDirectory) map {
      onDiskFiles =>
        // clear code that is not within the source directory
        val directorySource =
          sourceCode filter {
            state =>
              URIUtil.contains(sourceDirectory, state.fileURI)
          }

        onDiskFiles.foldLeft(directorySource) {
          case (currentCode, onDisk) =>
            currentCode putIfEmpty onDisk
        }
    }

  /**
   * Parse a source file, given its current sate.
   *
   * @param sourceState Current state of the source code
   * @param compiler    Target compiler
   * @return New source code state
   */
  @tailrec
  def parse(
      sourceState: SourceCodeState
    )(implicit file: FileAccess,
      compiler: CompilerAccess): SourceCodeState =
    sourceState match {
      case SourceCodeState.UnCompiled(fileURI, code) =>
        compiler.parseContracts(fileURI, code) match {
          case Left(error) =>
            SourceCodeState.ErrorParser(
              fileURI = fileURI,
              code = code,
              errors = Seq(error)
            )

          case Right(parsedCode) =>
            SourceCodeState.Parsed(
              fileURI = fileURI,
              code = code,
              ast = parsedCode
            )
        }

      case state @ (_: SourceCodeState.ErrorAccess | _: SourceCodeState.OnDisk) =>
        // access the code from disk and parse it.
        getSourceCode(state.fileURI) match {
          case state: SourceCodeState.UnCompiled =>
            // successfully accessed the code, now parse it.
            parse(state)

          case failed: SourceCodeState.ErrorAccess =>
            // Failed again: Return the error so the client gets reported.
            failed
        }

      case parsed @ (_: SourceCodeState.Parsed | _: SourceCodeState.Compiled) =>
        parsed // code is already in parsed state, return the same state

      case error: SourceCodeState.ErrorParser =>
        // This code was already contain parser errors, no need to reparse.
        error

      case error: SourceCodeState.ErrorCompilation =>
        // This code contains compilations errors that might have been fixed.
        // Since it is already parsed, return the existing parsed state, without re-parsing.
        error.parsed
    }

  /**
   * Insert the updated code for the given file-URI to the collection.
   *
   * Downgrade the current state of updated source-code so it gets re-parsed and re-compiled.
   * Also checks if the file is deleted so it could be removed from compilation.
   *
   * @param fileURI     Updated code's file-location
   * @param updatedCode The updated code
   * @param sourceCode  Existing source-code
   * @return New source code with applied change.
   */
  def putOrRemove(
      fileURI: URI,
      updatedCode: Option[String],
      sourceCode: ArraySeq[SourceCodeState]
    )(implicit file: FileAccess): ArraySeq[SourceCodeState] =
    updatedCode match {
      case Some(newCode) =>
        // new source code, store it as un-compiled.
        val newState =
          SourceCodeState.UnCompiled(fileURI, newCode)

        // update or add it to the existing collection
        sourceCode put newState

      case None =>
        // no source code sent from client, check it still exists.
        file.exists(fileURI, SourceIndexExtra.zero(fileURI)) match {
          case Left(error) =>
            // failed to check
            val newState =
              SourceCodeState.ErrorAccess(
                fileURI = fileURI,
                error = error
              )

            sourceCode put newState

          case Right(exists) =>
            if (exists) {
              // source-code exists, set it as on-disk so it gets read during the next parse & compilation.
              val newState =
                SourceCodeState.OnDisk(fileURI)

              sourceCode put newState
            } else {
              // file does not exist, remove it.
              sourceCode.filter(_.fileURI != fileURI)
            }
        }
    }

  /**
   * Compile a group of source-code files and performing type-check on imported code/import statements.
   *
   * @param sourceCode        Source-code to compile
   * @param compilerOptions   Options to run for this compilation
   * @param workspaceErrorURI URI to report errors that contain no `fileURI`.
   * @param compiler          Target compiler
   * @return Workspace-level error if an error occurred without a target source-file, or else next state for each source-code.
   */
  def compile(
      sourceCode: ArraySeq[SourceCodeState.Parsed],
      dependency: ArraySeq[SourceCodeState.Compiled],
      compilerOptions: CompilerOptions,
      workspaceErrorURI: URI
    )(implicit compiler: CompilerAccess,
      logger: ClientLogger): Either[CompilerMessage.AnyError, ArraySeq[SourceCodeState.IsParsed]] =
    Importer.typeCheck(
      sourceCode = sourceCode,
      dependency = dependency
    ) match {
      case Left(importErrorCode) =>
        // import type check resulted in errors. For example: It contains unknown imports.
        // merge existing source-code with errored source-code
        val newCode =
          sourceCode.merge(importErrorCode)(Ordering.by(_.fileURI))

        // new source-code with import errors
        Right(newCode)

      case Right(importedCode) =>
        // flatten the inheritance within dependency
        val importedTrees =
          flattenInheritance(
            toFlatten = importedCode.map(_.parsed),
            workspace = dependency.map(_.parsed)
          )

        val workspaceSourceTrees =
          SourceCodeSearcher.collectSourceTrees(sourceCode)

        compileSource(
          sourceTrees = workspaceSourceTrees,
          importedTrees = importedTrees,
          compilerOptions = compilerOptions,
          workspaceErrorURI = workspaceErrorURI
        )
    }

  /**
   * Compile a group of source-code files that are dependant on each other.
   *
   * Pre-requisite: It is assumed that imports are already processed.
   * If not, use [[SourceCode.compile]] instead.
   *
   * @param sourceTrees       Source-code to compile.
   * @param importedTrees     Imported source-code.
   * @param compilerOptions   Options to run for this compilation
   * @param workspaceErrorURI URI to report errors that contain no `fileURI`.
   * @param compiler          Target compiler
   * @return Workspace-level error if an error occurred without a target source-file, or else next state for each source-code.
   */
  private def compileSource(
      sourceTrees: ArraySeq[SourceLocation.Code],
      importedTrees: ArraySeq[SourceLocation.Code],
      compilerOptions: CompilerOptions,
      workspaceErrorURI: URI
    )(implicit compiler: CompilerAccess,
      logger: ClientLogger): Either[CompilerMessage.AnyError, ArraySeq[SourceCodeState.IsParsed]] = {
    val sourceTreesOnly =
      sourceTrees.map(_.tree)

    // Compile only the source-code. Import statements are already expected to be processed and included in `importedTrees` collection.
    val importedTreesOnly =
      importedTrees.map(_.tree)

    val allCode =
      sourceTreesOnly ++ importedTreesOnly

    val multiContractDef =
      allCode.map(_.ast)

    // compile the source-code
    val compilationResult =
      compiler.compileContracts(
        parsedSource = multiContractDef,
        options = compilerOptions,
        workspaceErrorURI = workspaceErrorURI
      )

    val uniqueFiles =
      sourceTrees.map(_.parsed).distinct

    // transform compilation result to SourceCodeState
    SourceCodeStateBuilder.toSourceCodeState(
      parsedCode = uniqueFiles,
      compilationResult = compilationResult,
      workspaceErrorURI = workspaceErrorURI
    )
  }

  private def getSourceCode(fileURI: URI)(implicit file: FileAccess): SourceCodeState.IsAccessed =
    file.read(fileURI) match {
      case Left(error) =>
        SourceCodeState.ErrorAccess(fileURI, error)

      case Right(sourceCode) =>
        SourceCodeState.UnCompiled(fileURI, sourceCode)
    }

  /**
   * Flattens the inheritance hierarchy.
   *
   * @param toFlatten The source files to flatten hierarchy, which may contain inheritance.
   * @param workspace The source files that contain implementations of all dependent code.
   * @return An array sequence of unique flattened source locations.
   */
  private def flattenInheritance(
      toFlatten: ArraySeq[SourceCodeState.Parsed],
      workspace: ArraySeq[SourceCodeState.Parsed]): ArraySeq[SourceLocation.Code] =
    if (toFlatten.isEmpty || workspace.isEmpty) {
      ArraySeq.empty
    } else {
      // Imports compiled ok. Compile source-code.
      val inheritedTrees =
        SourceCodeSearcher.collectInheritedParentsForAll(
          sourceCode = toFlatten,
          workspace = workspace
        )

      val usedCodeTrees =
        SourceCodeSearcher.collectSourceTrees(toFlatten)

      val allTrees =
        usedCodeTrees ++ inheritedTrees

      allTrees.distinct
    }

}
