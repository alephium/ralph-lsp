// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler

import org.alephium.ralph.Warning
import org.alephium.ralph.lsp.utils.log.LogMessage

/**
 * Log messages used by [[RalphCompilerAccess]].
 */
sealed trait CompilerLogMessage extends LogMessage {

  def message: String

}

object CompilerLogMessage {

  case class UnassignedWarningNoneFileURI(
      warning: Warning,
      contractName: String)
    extends CompilerLogMessage {

    def message =
      s"Unassigned Warning: `Warning.SourceIndex.fileURI` & `Contract.SourceIndex.fileURI` are `None`. Contract: $contractName. Warning: $warning"

  }

  case class UnassignedWarningNoneFileInfo(
      warning: Warning,
      contractName: String)
    extends CompilerLogMessage {

    def message =
      s"Unassigned Warning: `Warning.SourceIndex`, `Contract.Ident.SourceIndex` & `Contract.SourceIndex` are `None`. Contract: $contractName. Warning: $warning"

  }

  case class NoneIdentSourceIndex(
      warning: Warning,
      contractName: String)
    extends CompilerLogMessage {

    def message =
      s"Contract '$contractName' has undefined SourceIndex for its Ident. Reporting warning at Contract's SourceIndex. Warning: $warning"

  }

  case class UnassignedWarning(warning: Warning) extends CompilerLogMessage {

    def message =
      s"Unassigned Warning: Missing file information. Warning: $warning"

  }

}
