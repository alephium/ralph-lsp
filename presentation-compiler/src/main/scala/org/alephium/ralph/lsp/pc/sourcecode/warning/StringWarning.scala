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

package org.alephium.ralph.lsp.pc.sourcecode.warning

import org.alephium.ralph
import org.alephium.ralph.SourceIndex
import org.alephium.ralph.lsp.access.compiler.message.{CompilerMessage, SourceIndexExtra}
import org.alephium.ralph.lsp.utils.log.{ClientLogger, StrictImplicitLogging}

import java.net.URI

/**
 * String warning reported by `ralphc` not containing source-location information.
 */
object StringWarning extends StrictImplicitLogging {

  @inline def apply(
      message: String,
      fileURI: URI): StringWarning =
    StringWarning(
      message = message,
      index = SourceIndexExtra.zero(fileURI)
    )

  /**
   * Creates an instance of [[StringWarning]] with the specified file [[URI]].
   *
   * @param warning [[ralph.Warning]] to convert into a [[StringWarning]].
   * @param fileURI File [[URI]] where this warning is to be reported.
   * @return A [[StringWarning]] with the specified file [[URI]] set.
   */
  def apply(
      warning: ralph.Warning,
      fileURI: URI
    )(implicit logger: ClientLogger): StringWarning =
    warning.sourceIndex match {
      case Some(sourceIndex) =>
        sourceIndex.fileURI match {
          case Some(sourceIndexFileURI) => // SourceIndex contains a fileURI.
            // There should never be a case where the fileURIs are different.
            // But this check is performed to detect bugs; Warnings should always be mapped to the right fileURI.
            if (sourceIndexFileURI != fileURI)
              logger.error(s"Warning-URI '$fileURI' is not equal to Source-URI '$sourceIndexFileURI'. Message: ${warning.message}")

            StringWarning(
              message = warning.message,
              index = sourceIndex
            )

          case None =>
            // fileURI is not provided in SourceIndex, use the one provided as input.
            StringWarning(
              message = warning.message,
              index = sourceIndex.copy(fileURI = Some(fileURI))
            )
        }

      case None =>
        StringWarning(
          message = warning.message,
          fileURI = fileURI
        )
    }

}

case class StringWarning(
    message: String,
    index: SourceIndex)
  extends CompilerMessage.Warning
