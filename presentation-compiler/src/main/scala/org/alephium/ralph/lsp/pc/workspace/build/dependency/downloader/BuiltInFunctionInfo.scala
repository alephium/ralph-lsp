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

package org.alephium.ralph.lsp.pc.workspace.build.dependency.downloader

import org.alephium.ralph.BuiltIn
import org.alephium.ralph.BuiltIn.Category

object BuiltInFunctionInfo {

  /**
   * This code is borrowed from `alephium-tools` project.
   *
   * @see <a href="https://github.com/alephium/alephium/blob/master/tools/src/main/scala/org/alephium/tools/BuiltInFunctions.scala">BuiltInFunctions.scala</a>
   */
  implicit val ordering: Ordering[BuiltInFunctionInfo] = {
    val orders =
      Seq[BuiltIn.Category](
        Category.Contract,
        Category.SubContract,
        Category.Asset,
        Category.Utils,
        Category.Chain,
        Category.Conversion,
        Category.ByteVec,
        Category.Cryptography
      )

    Ordering.by {
      functionInfo =>
        orders.indexOf(functionInfo.category)
    }
  }

  /**
   * Builds a sequence of [[BuiltInFunctionInfo]] instances representing built-in functions supported by Ralph.
   */
  def build(): Seq[BuiltInFunctionInfo] =
    BuiltIn
      .statefulFuncsSeq
      .map {
        case (_, function) =>
          BuiltInFunctionInfo(
            name = function.name,
            category = function.category,
            signature = function.signature,
            doc = function.doc,
            params = function.params,
            returns = function.returns
          )
      }

}

/**
 * Represents information about a built-in function.
 *
 * @param name      The name of the built-in function.
 * @param category  The category of the built-in function.
 * @param signature The signature of the built-in function.
 * @param doc       The documentation of the built-in function.
 * @param params    The parameters of the built-in function.
 * @param returns   The return type of the built-in function.
 */
final case class BuiltInFunctionInfo(
    name: String,
    category: Category,
    signature: String,
    doc: String,
    params: Seq[String],
    returns: String)
