package org.alephium.ralph.lsp.pc.workspace.build.dependency

import scala.collection.immutable.ArraySeq

sealed trait DependencyID {
  def dirName: String
}

object DependencyID {

  case object Std extends DependencyID {
    override def dirName: String = "std"
  }

  case object BuiltIn extends DependencyID {
    override def dirName: String = "built_in"
  }

  def all(): ArraySeq[DependencyID] =
    ArraySeq(
      DependencyID.Std,
      DependencyID.BuiltIn
    )

}
