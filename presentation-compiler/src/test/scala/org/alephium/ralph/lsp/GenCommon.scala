package org.alephium.ralph.lsp

import org.scalacheck.Gen

/**
 * Common test data generator used by all other data types.
 */
object GenCommon {
  /** A random name. Restricted to 10 characters. */
  val genName: Gen[String] =
    Gen.listOfN(10, Gen.alphaChar).map(_.mkString)

  val genCamelCase: Gen[String] =
    genName.map(_.capitalize)

}
