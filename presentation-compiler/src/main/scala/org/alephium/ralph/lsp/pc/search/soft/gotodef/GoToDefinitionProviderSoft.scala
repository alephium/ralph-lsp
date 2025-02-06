package org.alephium.ralph.lsp.pc.search.soft.gotodef

import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.pc.search.CodeProvider
import org.alephium.ralph.lsp.pc.search.gotodef.GoToDefSetting
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.WorkspaceState
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}
import org.alephium.ralph.lsp.utils.Node

case object GoToDefinitionProviderSoft extends CodeProvider[SourceCodeState.IsParsed, (SoftAST.type, GoToDefSetting), SourceLocation.GoToDefSoft] with StrictImplicitLogging {

  /** @inheritdoc */
  override def search(
      cursorIndex: Int,
      sourceCode: SourceCodeState.IsParsed,
      workspace: WorkspaceState.IsSourceAware,
      searchSettings: (SoftAST.type, GoToDefSetting)
    )(implicit logger: ClientLogger): Iterator[SourceLocation.GoToDefSoft] =
    sourceCode.astSoft.fetch() match {
      case Left(error) =>
        // This will be removed when integration is complete,
        // when SourceCodeState.ErrorParser responds to SoftParser errors.
        // Note: SoftParser is not expected to fail given any input, so this is less likely to occur.
        //       Log it for now.
        logger.error {
          s"""SoftParser Error: Failed to parse source code.
             |File: ${sourceCode.fileURI}
             |Error Message: ${error.message}""".stripMargin
        }

        Iterator.empty

      case Right(softAST) =>
        // First, find the first code block where the cursorIndex belongs, i.e. [[SoftAST.BodyPartAST]].
        // In a well-defined code, this is expected to be a top level Contract [[SoftAST.Template]].
        softAST.toNode.data.parts.find(_.index contains cursorIndex) match {
          case Some(bodyPart) =>
            searchBodyPart(
              cursorIndex = cursorIndex,
              bodyPart = bodyPart,
              sourceCode = sourceCode
            )

          case None =>
            Iterator.empty
        }
    }

  /**
   * Searches the given bodyPart.
   *
   * @param cursorIndex The index location where the search operation is performed.
   * @param bodyPart    The first code block where the search is executed.
   * @param sourceCode  The parsed state of the source-code where the search is executed.
   */
  private def searchBodyPart(
      cursorIndex: Int,
      bodyPart: SoftAST.BlockBodyPart,
      sourceCode: SourceCodeState.IsParsed
    )(implicit logger: ClientLogger): Iterator[SourceLocation.GoToDefSoft] =
    bodyPart.toNode.findLast(_.index contains cursorIndex) match {
      case Some(Node(_: SoftAST.CodeToken[_], _)) =>
        // Tokens (fn, Contract etc.) do not require go-to-definitions
        Iterator.empty

      case Some(node @ Node(codeString: SoftAST.CodeString, _)) =>
        GoToDefCodeString(
          node = node.upcast(codeString),
          sourceCode = SourceLocation.CodeSoft(bodyPart, sourceCode)
        )

      case Some(node) =>
        logger.trace(s"Go-to-definition not implemented for AST '${node.data.getClass.getSimpleName}' at source index '${node.data.index}'. File: ${sourceCode.fileURI}")
        Iterator.empty

      case None =>
        // If this occurs, the client must be sending requests with incorrect 'cursorIndex'.
        logger.error(s"AST Node not found for index $cursorIndex. File: ${sourceCode.fileURI}")
        Iterator.empty
    }

}
