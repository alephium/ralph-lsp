// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace.build.dependency

import scala.collection.immutable.ArraySeq

sealed trait DependencyID extends Product {

  final def dirName: String =
    productPrefix.toLowerCase

}

object DependencyID {

  case object Std     extends DependencyID
  case object BuiltIn extends DependencyID

  def all(): ArraySeq[DependencyID] =
    ArraySeq(
      DependencyID.Std,
      DependencyID.BuiltIn
    )

}
