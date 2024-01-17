package org.alephium.ralph.lsp

import org.scalacheck.Gen

object GenExtension {

  /** Extension functions to ScalaCheck [[Gen]] */
  implicit class GenExtensionsImplicits(val gen: Gen.type) extends AnyVal {

    /** Similar to [[Gen.listOfN]] but limits the maximum */
    def listOfMax[T](max: Int = 10)(gen: Gen[T]): Gen[List[T]] =
      for {
        maxOf <- Gen.choose(0, max)
        list <- Gen.listOfN(maxOf, gen)
      } yield list
  }

}
