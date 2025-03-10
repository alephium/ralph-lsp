// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

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
