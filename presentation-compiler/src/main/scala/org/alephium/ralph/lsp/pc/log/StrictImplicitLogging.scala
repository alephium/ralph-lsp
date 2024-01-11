package org.alephium.ralph.lsp.pc.log

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

/** Copy of [[com.typesafe.scalalogging.StrictLogging]] */
trait StrictImplicitLogging {

  protected implicit val loggerSLF4J: Logger =
    Logger(LoggerFactory.getLogger(getClass.getName))

}
