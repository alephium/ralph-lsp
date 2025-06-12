// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

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
      .:+(buildEncodeFields())

  /**
   * [[BuiltIn.statefulFuncsSeq]] does not include the static function `encodeFields`.
   * This manually builds it within the category [[Category.Contract]].
   *
   * Documentation borrowed from <a href="https://docs.alephium.org/ralph/built-in-functions/#encodefields">#encodefields</a>.
   *
   * @return Built-in function information for `encodeFields!`.
   */
  private def buildEncodeFields(): BuiltInFunctionInfo = {
    val name = "encodeFields"

    BuiltInFunctionInfo(
      name = name,
      category = Category.Contract,
      signature = s"fn $name!(fields:Fields) -> (ByteVec, ByteVec)",
      doc = "Encode the fields for creating a contract",
      params = Seq("@param fields the fields of the to-be-created target contract"),
      returns = "@returns two ByteVecs: the first one is the encoded immutable fields, and the second one is the encoded mutable fields"
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
