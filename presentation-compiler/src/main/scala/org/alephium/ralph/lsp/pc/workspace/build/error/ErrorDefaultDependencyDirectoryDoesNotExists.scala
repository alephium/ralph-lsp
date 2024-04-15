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

package org.alephium.ralph.lsp.pc.workspace.build.error

import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.CompilerMessage
import org.alephium.ralph.lsp.pc.workspace.build.Build

case class ErrorDefaultDependencyDirectoryDoesNotExists(index: SourceIndex) extends CompilerMessage.Error {

  override def message: String =
    s"""Unable to write dependencies because `dependencyPath` or the system property `user.home` is not configured.
       |To fix this, either add the setting "dependencyPath": "dependencies" in ${Build.BUILD_FILE_NAME} and create a directory named "dependencies" in your project, or set the system property `user.home`.
       |""".stripMargin

}
