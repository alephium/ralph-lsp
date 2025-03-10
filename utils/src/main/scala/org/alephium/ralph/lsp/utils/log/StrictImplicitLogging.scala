// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.utils.log

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

/** Copy of [[com.typesafe.scalalogging.StrictLogging]] */
trait StrictImplicitLogging {

  protected implicit val loggerSLF4J: Logger =
    Logger(LoggerFactory.getLogger(getClass.getName))

}
