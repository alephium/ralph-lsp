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

package org.alephium.ralph.lsp.pc.diagnostic

sealed trait CodeDiagnosticSeverity

/**
 * TODO: This type should be removed and [[CodeDiagnostic]] should provide Error and Warning types
 *       This temporarily exists because [[CodeDiagnostic]] was moved from `lsp-server`'s LSP4J version
 *       with minimal changes and LSP4J has the type `DiagnosticSeverity`.
 */
object CodeDiagnosticSeverity {

  case object Error   extends CodeDiagnosticSeverity
  case object Warning extends CodeDiagnosticSeverity

}
