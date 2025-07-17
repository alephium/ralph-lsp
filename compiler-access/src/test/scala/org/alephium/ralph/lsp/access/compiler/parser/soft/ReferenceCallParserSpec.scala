// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ReferenceCallParserSpec extends AnyWordSpec with Matchers {

  "reference call with asset approval" in {
    val ast =
      parseReferenceCall("function { user -> 1 }()")

    ast shouldBe
      SoftAST.ReferenceCall(
        index = indexOf(">>function { user -> 1 }()<<"),
        reference = Identifier(">>function<< { user -> 1 }()"),
        preAssetApprovalSpace = Some(Space("function>> <<{ user -> 1 }()")),
        assetApproval = Some(
          SoftAST.AssetApproval(
            SoftAST.Group(
              index = indexOf("function >>{ user -> 1 }<<()"),
              openToken = Some(OpenCurly("function >>{<< user -> 1 }()")),
              preHeadExpressionSpace = Some(Space("function {>> <<user -> 1 }()")),
              headExpression = Some(
                SoftAST.Group(
                  index = indexOf("function { >>user -> 1<< }()"),
                  openToken = None,
                  preHeadExpressionSpace = None,
                  headExpression = Some(
                    SoftAST.ArrowAssignment(
                      index = indexOf("function { >>user -> 1<< }()"),
                      leftExpression = Identifier("function { >>user<< -> 1 }()"),
                      preArrowSpace = Some(Space("function { user>> <<-> 1 }()")),
                      forwardArrow = ForwardArrow("function { user >>-><< 1 }()"),
                      preRightExpressionSpace = Some(Space("function { user - >> <<1 }()")),
                      rightExpression = Number("function { user -> >>1<< }()")
                    )
                  ),
                  preTailExpressionSpace = None,
                  tailExpressions = Seq.empty,
                  closeToken = None
                )
              ),
              preTailExpressionSpace = Some(Space("function { user -> 1>> <<}()")),
              tailExpressions = Seq.empty,
              closeToken = Some(CloseCurly("function { user -> 1 >>}<<()"))
            )
          )
        ),
        preArgumentsSpace = None,
        arguments = SoftAST.Group(
          index = indexOf("function { user -> 1 }>>()<<"),
          openToken = Some(OpenParen("function { user -> 1 }>>(<<)")),
          preHeadExpressionSpace = None,
          headExpression = None,
          preTailExpressionSpace = None,
          tailExpressions = Seq.empty,
          closeToken = Some(CloseParen("function { user -> 1 }(>>)<<"))
        )
      )
  }

}
