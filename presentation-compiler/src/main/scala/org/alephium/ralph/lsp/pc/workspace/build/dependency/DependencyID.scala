// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace.build.dependency

import org.alephium.ralph.lsp.BuildInfo

import scala.collection.immutable.ArraySeq

sealed abstract class DependencyID(version: String) extends Product {

  final val importName: String =
    productPrefix.toLowerCase

  final val dirName: String =
    s"$importName-$version"

}

object DependencyID {

  case object Std     extends DependencyID(BuildInfo.web3Version)
  case object BuiltIn extends DependencyID(BuildInfo.ralphcVersion)

  def all(): ArraySeq[DependencyID] =
    ArraySeq(
      DependencyID.Std,
      DependencyID.BuiltIn
    )

}
