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
