package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.pc.sourcecode.imports.Importer
import org.alephium.ralph.lsp.pc.util.CollectionUtil._
import org.alephium.ralph.lsp.pc.util.URIUtil

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
      .map(_.map(SourceCodeState.OnDisk).to(ArraySeq))

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
  def synchronise(sourceDirectory: URI,
                  sourceCode: ArraySeq[SourceCodeState])(implicit file: FileAccess): Either[CompilerMessage.AnyError, ArraySeq[SourceCodeState]] =
    initialise(sourceDirectory) map {
      onDiskFiles =>
        // clear code that is no within the source directory
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
  def parse(sourceState: SourceCodeState)(implicit file: FileAccess,
                                          compiler: CompilerAccess): SourceCodeState =
    sourceState match {
      case SourceCodeState.UnCompiled(fileURI, code) =>
        compiler.parseContracts(code) match {
          case Left(error) =>
            SourceCodeState.ErrorSource(
              fileURI = fileURI,
              code = code,
              errors = Array(error),
              previous = None
            )

          case Right(parsedCode) =>
            SourceCodeState.Parsed(
              fileURI = fileURI,
              code = code,
              ast = parsedCode,
            )
        }

      case onDisk: SourceCodeState.OnDisk =>
        getSourceCode(onDisk.fileURI) match {
          case errored: SourceCodeState.IsError =>
            errored

          case gotCode =>
            parse(gotCode)
        }

      case accessError: SourceCodeState.ErrorAccess =>
        // access the code from disk and parse it.
        getSourceCode(accessError.fileURI) match {
          case state: SourceCodeState.UnCompiled =>
            // successfully accessed the code, now parse it.
            parse(state)

          case failed: SourceCodeState.ErrorAccess =>
            // Failed again: Return the error so the client gets reported.
            failed
        }

      case parsed @ (_: SourceCodeState.Parsed | _: SourceCodeState.Compiled) =>
        parsed // code is already in parsed state, return the same state

      case error: SourceCodeState.ErrorSource =>
        // Code was already parsed and it errored.
        // Return the same state.
        error
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
  def putOrRemove(fileURI: URI,
                  updatedCode: Option[String],
                  sourceCode: ArraySeq[SourceCodeState])(implicit file: FileAccess): ArraySeq[SourceCodeState] =
    updatedCode match {
      case Some(newCode) =>
        // new source code, store it as un-compiled.
        val newState =
          SourceCodeState.UnCompiled(fileURI, newCode)

        // update or add it to the existing collection
        sourceCode put newState

      case None =>
        // no source code sent from client, check it still exists.
        file.exists(fileURI) match {
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
   * @param sourceCode      Source-code to compile
   * @param compilerOptions Options to run for this compilation
   * @param compiler        Target compiler
   * @return Workspace-level error if an error occurred without a target source-file, or else next state for each source-code.
   */
  def compile(sourceCode: ArraySeq[SourceCodeState.Parsed],
              dependency: Option[ArraySeq[SourceCodeState.Compiled]],
              compilerOptions: CompilerOptions)(implicit compiler: CompilerAccess): Either[CompilerMessage.AnyError, ArraySeq[SourceCodeState.IsParsed]] =
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
        // Imports compiled ok. Compile source-code.
        val parsedImported =
          importedCode.map(_.parsed)

        compileSource(
          sourceCode = sourceCode,
          importedCode = parsedImported,
          compilerOptions = compilerOptions
        )
    }

  /**
   * Compile a group of source-code files that are dependant on each other.
   *
   * Pre-requisite: It is assumed that imports are already processed.
   * If not, use [[SourceCode.compile]] instead.
   *
   * @param sourceCode      Source-code to compile
   * @param compilerOptions Options to run for this compilation
   * @param compiler        Target compiler
   * @return Workspace-level error if an error occurred without a target source-file, or else next state for each source-code.
   */
  private def compileSource(sourceCode: ArraySeq[SourceCodeState.Parsed],
                            importedCode: ArraySeq[SourceCodeState.Parsed],
                            compilerOptions: CompilerOptions)(implicit compiler: CompilerAccess): Either[CompilerMessage.AnyError, ArraySeq[SourceCodeState.IsCompiled]] = {
    val allCode =
      sourceCode ++ importedCode

    // Compile only the source-code. Import statements are already expected to be processed and included in `importedCode` collection.
    val contractsToCompile =
      allCode flatMap {
        state =>
          // collect only the source-code ignoring all import statements.
          state.ast.statements collect {
            case source: Tree.Source =>
              source.ast
          }
      }

    // compile the source-code
    val compilationResult =
      compiler.compileContracts(
        contracts = contractsToCompile,
        options = compilerOptions
      )

    // transform compilation result to SourceCodeState
    SourceCodeStateBuilder.toSourceCodeState(
      parsedCode = sourceCode,
      compilationResult = compilationResult
    )
  }

  private def getSourceCode(fileURI: URI)(implicit file: FileAccess): SourceCodeState.IsAccessed =
    file.read(fileURI) match {
      case Left(error) =>
        SourceCodeState.ErrorAccess(fileURI, error)

      case Right(sourceCode) =>
        SourceCodeState.UnCompiled(fileURI, sourceCode)
    }

}
