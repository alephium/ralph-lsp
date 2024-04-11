package org.alephium.ralph.lsp.server.state

import org.alephium.ralph.lsp.server.ResponseError.InvalidTraceSetting

/**
 * @see [[org.eclipse.lsp4j.TraceValue]].
 */
sealed trait Trace

object Trace {

  case object Off      extends Trace
  case object Messages extends Trace
  case object Verbose  extends Trace

  def apply(string: String): Either[InvalidTraceSetting, Trace] =
    if (string == null)
      Right(Trace.Off)
    else if (string equalsIgnoreCase Trace.Off.productPrefix)
      Right(Trace.Off)
    else if (string equalsIgnoreCase Trace.Messages.productPrefix)
      Right(Trace.Messages)
    else if (string equalsIgnoreCase Trace.Verbose.productPrefix)
      Right(Trace.Verbose)
    else
      Left(InvalidTraceSetting(string))

}
