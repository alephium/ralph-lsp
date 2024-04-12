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

    override def dirName: String = "builtin"

  }

  def all(): ArraySeq[DependencyID] =
    ArraySeq(
      DependencyID.Std,
      DependencyID.BuiltIn
    )

}
