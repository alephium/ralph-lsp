package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.Ast.ContractWithState
import org.alephium.ralph.lsp.pc.sourcecode.imports.{ImportHandler, ImportState}
import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.workspace.build.BuildDependencies
import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.pc.util.CollectionUtil._
import org.alephium.ralph.lsp.pc.util.URIUtil

import java.net.URI
import scala.annotation.tailrec
import scala.collection.immutable.{ArraySeq, ListMap}

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
        parseContractsAndImports(code) match {
          case Left(errors) =>
            SourceCodeState.ErrorSource(
              fileURI = fileURI,
              code = code,
              errors = errors,
              previous = None
            )

          case Right((parsedCode, parsedImports)) =>
            SourceCodeState.Parsed(
              fileURI = fileURI,
              code = code,
              contracts = parsedCode,
              imports = parsedImports
            )
        }

      case onDisk: SourceCodeState.OnDisk =>
        getSourceCode(onDisk.fileURI) match {
          case errored: SourceCodeState.FailedState =>
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
   * Compile a group of source-code files that are dependant on each other.
   *
   * @param sourceCode      Source-code to compile
   * @param compilerOptions Options to run for this compilation
   * @param compiler        Target compiler
   * @return Optional workspace-level error if an error occurred without a target source-file and the next state for each source-code, which could also contains errors.
   */
  def compile(sourceCode: ArraySeq[SourceCodeState.Parsed],
              compilerOptions: CompilerOptions,
              buildDependencies: BuildDependencies)(implicit compiler: CompilerAccess): (Option[CompilerMessage.AnyError], ArraySeq[SourceCodeState.CodeAware]) = {
    val contractsToCompile =
      sourceCode.flatMap(_.contracts)

    val importsErrorAndAst = compileImports(sourceCode, buildDependencies)

    val importErrors = importsErrorAndAst.map{ case (error,_) => error }.flatten
    //`distinct` as compiler will fail if we import the same interface in different files
    val importsAst = importsErrorAndAst.map{ case (_, ast) => ast }.distinct.flatten

    val compilationResult =
      compiler.compileContracts(
        contracts = contractsToCompile ++ importsAst.flatMap(_.compiledCode),
        options = compilerOptions
      )

    SourceCodeStateBuilder.toSourceCodeState(
      parsedCode = sourceCode,
      compilationResult = compilationResult
    ) match {
      //Import errors are put back after compilation happen, as their are not handled by ralphc
      case Left(error) => (Some(error), mergeSourceCodeAndErrors(sourceCode, importErrors))
      case Right(codeAwares) => (None, mergeSourceCodeAndErrors(codeAwares, importErrors))
    }
  }

  //Errors needs to replace initial parsed sourceCodes
  private def mergeSourceCodeAndErrors(sourceCode: ArraySeq[SourceCodeState.CodeAware], errors: Seq[SourceCodeState.ErrorSource]): ArraySeq[SourceCodeState.CodeAware] =  {
    //Using List map to preserve order
    val sourceCodeMap: ListMap[URI, SourceCodeState.CodeAware] = ListMap.from(sourceCode.map{sc => (sc.fileURI, sc)})
    ArraySeq.from(errors.foldLeft(sourceCodeMap){ case (acc, error) =>
      acc.updated(error.fileURI, error)
    }.values)
  }

  //Imports are a bit different than full source code compiling, we want to find all invalid import as well as returning the AST for the valid ones.
  def compileImports(sourceCode:ArraySeq[SourceCodeState.Parsed],buildDependencies: BuildDependencies)(implicit compiler: CompilerAccess): ArraySeq[(Option[SourceCodeState.ErrorSource], Seq[ImportState.Compiled])] = {
    sourceCode.map { sourceCode =>
      val (errors, validImports) = ImportHandler.compileImports(sourceCode.imports)(compiler,buildDependencies).partitionMap(identity)

      val errorSource = Option.when(errors.nonEmpty){
        SourceCodeState.ErrorSource(
          sourceCode.fileURI,
          sourceCode.code,
          errors,
          Some(sourceCode)
        )
      }

      (errorSource, validImports.distinct)
    }
  }

  private def parseContractsAndImports(code: String)(implicit compiler: CompilerAccess): Either[Seq[CompilerMessage.AnyError], (Seq[ContractWithState], Seq[ImportState.Parsed])] =
    for {
      codeWithImports <- imports.ImportHandler.parseImports(code)
      codeAst <- compiler.parseContracts(codeWithImports.code).left.map(Seq(_))
    } yield {
      (codeAst, codeWithImports.parsedImports)
    }

  private def getSourceCode(fileURI: URI)(implicit file: FileAccess): SourceCodeState.AccessedState =
    file.read(fileURI) match {
      case Left(error) =>
        SourceCodeState.ErrorAccess(fileURI, error)

      case Right(sourceCode) =>
        SourceCodeState.UnCompiled(fileURI, sourceCode)
    }

}
