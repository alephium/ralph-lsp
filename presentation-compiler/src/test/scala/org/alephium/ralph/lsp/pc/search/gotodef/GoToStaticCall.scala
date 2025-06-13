// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.alephium.ralph.lsp.pc.search.TestCodeProvider._

class GoToStaticCall extends AnyWordSpec with Matchers {

  "access single static functions" in {
    goToDefinitionSoft()(
      """
        |Contract Test {
        |  fn >>encodeFields<<() -> ()
        |}
        |
        |Test.encodeFiel@@ds
        |""".stripMargin
    )
  }

  "access duplicate static functions" in {
    goToDefinitionSoft()(
      """
        |Contract Test {
        |  fn >>encodeFields<<() -> ()
        |  fn >>encodeFields<<(paramA: A) -> ()
        |  fn >>encodeFields<<(paramA: A, paramB: A) -> ()
        |}
        |
        |Test.encodeFiel@@ds
        |""".stripMargin
    )
  }

  "access within another contract" in {
    goToDefinitionSoft()(
      """
        |Contract Test {
        |  fn >>encodeFields<<() -> ()
        |  fn >>encodeFields<<(paramA: A) -> ()
        |  fn >>encodeFields<<(paramA: A, paramB: A) -> ()
        |}
        |
        |Contract Main {
        |  Test.encodeFiel@@ds
        |}
        |""".stripMargin
    )
  }

}
