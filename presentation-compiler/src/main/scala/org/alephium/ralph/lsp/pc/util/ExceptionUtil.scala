package org.alephium.ralph.lsp.pc.util

import java.io.{PrintWriter, StringWriter}

object ExceptionUtil {

  /** Emit stack-trace as String */
  def toStringStackTrace(throwable: Throwable): String = {
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    throwable.printStackTrace(pw)
    sw.toString
  }

  /** Merge a message and exception into a String */
  def mergeToString(message: String, cause: Throwable): String = {
    val stringStackTrace = toStringStackTrace(cause)

    s"""$message
       |Cause: $stringStackTrace""".stripMargin
  }

}
