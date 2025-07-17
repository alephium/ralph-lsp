// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.alephium.ralph.lsp.access.util.TestFastParse._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class EmitParserSpec extends AnyWordSpec with Matchers {

  "fail" when {
    "`emit` is not a keyword" in {
      assertIsFastParseError {
        parseEmit("emitter")
      }
    }
  }

  "pass" when {
    "only `emit` is defined" in {
      val emit = parseEmit("emit")

      emit shouldBe
        SoftAST.Emit(
          index = indexOf(">>emit<<"),
          emit = Emit(">>emit<<"),
          preExpressionSpace = None,
          expression = ExpressionExpected("emit>><<")
        )
    }

    "emit identifier" in {
      val emit = parseEmit("emit ABC")

      emit shouldBe
        SoftAST.Emit(
          index = indexOf(">>emit ABC<<"),
          emit = Emit(">>emit<< ABC"),
          preExpressionSpace = Some(Space("emit>> <<ABC")),
          expression = Identifier("emit >>ABC<<")
        )
    }

    "emit reference call" in {
      val emit = parseEmit("emit createEvent()")

      emit shouldBe
        SoftAST.Emit(
          index = indexOf(">>emit createEvent()<<"),
          emit = Emit(">>emit<< createEvent()"),
          preExpressionSpace = Some(Space("emit>> <<createEvent()")),
          expression = SoftAST.ReferenceCall(
            index = indexOf("emit >>createEvent()<<"),
            reference = Identifier("emit >>createEvent<<()"),
            preArgumentsSpace = None,
            arguments = SoftAST.Group(
              index = indexOf("emit createEvent>>()<<"),
              openToken = Some(OpenParen("emit createEvent>>(<<)")),
              preHeadExpressionSpace = None,
              headExpression = None,
              preTailExpressionSpace = None,
              tailExpressions = Seq.empty,
              closeToken = Some(CloseParen("emit createEvent(>>)<<"))
            )
          )
        )
    }

    "emit method call" in {
      val emit = parseEmit("emit contract.createEvent()")

      emit shouldBe
        SoftAST.Emit(
          index = indexOf(">>emit contract.createEvent()<<"),
          emit = Emit(">>emit<< contract.createEvent()"),
          preExpressionSpace = Some(Space("emit>> <<contract.createEvent()")),
          expression = SoftAST.MethodCall(
            index = indexOf("emit >>contract.createEvent()<<"),
            leftExpression = Identifier("emit >>contract<<.createEvent()"),
            preDotSpace = None,
            dot = Dot("emit contract>>.<<createEvent()"),
            preRightExpressionSpace = None,
            rightExpression = SoftAST.ReferenceCall(
              index = indexOf("emit contract.>>createEvent()<<"),
              reference = Identifier("emit contract.>>createEvent<<()"),
              preArgumentsSpace = None,
              arguments = SoftAST.Group(
                index = indexOf("emit contract.createEvent>>()<<"),
                openToken = Some(OpenParen("emit contract.createEvent>>(<<)")),
                preHeadExpressionSpace = None,
                headExpression = None,
                preTailExpressionSpace = None,
                tailExpressions = Seq.empty,
                closeToken = Some(CloseParen("emit contract.createEvent(>>)<<"))
              )
            )
          )
        )
    }
  }

}
