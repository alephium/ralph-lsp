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

package org.alephium.ralph.lsp.pc.util

import org.alephium.ralph.CompilerOptions
import org.alephium.ralph.lsp.pc.workspace.build.config.{RalphcConfigState, CompilerOptionsParsed}
import upickle.default._

import java.nio.file.{Path, Paths}

object PicklerUtil {

  implicit val pathReader: ReadWriter[Path] =
    readwriter[String].bimap(_.toString, Paths.get(_))

  implicit val optionStringReader: ReadWriter[Option[String]] =
    readwriter[String].bimap(
      value => value.orNull,
      string => Option(string)
    )

  // TODO: Required for upickle version 3.3.0. Not required in 4.0.0. Remove when upgraded.
  implicit val optionBooleanReader: ReadWriter[Option[Boolean]] =
    readwriter[Boolean].bimap(
      value => value.getOrElse(false),
      boolean => Option(boolean)
    )

  implicit val compilerOptionsReaderWriter: ReadWriter[CompilerOptions] =
    macroRW

  implicit val configJSONReaderWriter: ReadWriter[CompilerOptionsParsed] =
    macroRW

  // TODO: Required for upickle version 3.3.0. Not required in 4.0.0. Remove when upgraded.
  implicit val optionJSONConfigReaderWriter: ReadWriter[Option[CompilerOptionsParsed]] =
    readwriter[CompilerOptionsParsed].bimap(
      value => value.orNull,
      string => Option(string)
    )

  implicit val ralphcConfigReaderWriter: ReadWriter[RalphcConfigState.Parsed] =
    macroRW

}
