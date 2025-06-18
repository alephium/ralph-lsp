// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StringInterpolationParserSpec extends AnyWordSpec with Matchers {

  "single tick (no closing tick)" in {
    val ast = parseStringInterpolation("`")

    val expected =
      SoftAST.StringInterpolation(
        index = indexOf(">>`<<"),
        startTick = Tick(">>`<<"),
        interpolation = Seq.empty,
        endTick = TokenExpected("`>><<", Token.Tick)
      )

    ast shouldBe expected
  }

  "empty string" in {
    val ast = parseStringInterpolation("``")

    val expected =
      SoftAST.StringInterpolation(
        index = indexOf(">>``<<"),
        startTick = Tick(">>`<<`"),
        interpolation = Seq.empty,
        endTick = Tick("`>>`<<").code
      )

    ast shouldBe expected
  }

  "escaped interpolated string" when {
    "$$" in {
      val ast = parseStringInterpolation("`$${a + b}`")

      val expected =
        SoftAST.StringInterpolation(
          index = indexOf(">>`$${a + b}`<<"),
          startTick = Tick(">>`<<$${a + b}`"),
          interpolation = Seq(
            Left(
              Seq(
                Dollar("`>>$<<${a + b}`").code,
                Dollar("`$>>$<<{a + b}`").code,
                CodeString("`$$>>{a + b}<<`")
              )
            )
          ),
          endTick = Tick("`$${a + b}>>`<<").code
        )

      ast shouldBe expected
    }

    "$$$$" in {
      val ast = parseStringInterpolation("`$$$${a + b}`")

      val expected =
        SoftAST.StringInterpolation(
          index = indexOf(">>`$$$${a + b}`<<"),
          startTick = Tick(">>`<<$$$${a + b}`"),
          interpolation = Seq(
            Left(
              Seq(
                Dollar("`>>$<<$$${a + b}`").code,
                Dollar("`$>>$<<$${a + b}`").code,
                Dollar("`$$>>$<<${a + b}`").code,
                Dollar("`$$$>>$<<{a + b}`").code,
                CodeString("`$$$$>>{a + b}<<`")
              )
            )
          ),
          endTick = Tick("`$$$${a + b}>>`<<").code
        )

      ast shouldBe expected
    }
  }

  "missing block" when {
    "block is missing" in {
      val ast = parseStringInterpolation("`$`")

      val expected =
        SoftAST.StringInterpolation(
          index = indexOf(">>`$`<<"),
          startTick = Tick(">>`<<$`"),
          interpolation = Seq(
            Right(
              Seq(
                SoftAST.InterpolatedBlock(
                  index = indexOf("`>>$<<`"),
                  dollar = Dollar("`>>$<<`").code,
                  block = SoftAST.BlockExpected(indexOf("`$>><<`"))
                )
              )
            )
          ),
          endTick = Tick("`$>>`<<").code
        )

      ast shouldBe expected
    }

    "tail block is missing" in {
      val ast = parseStringInterpolation("`${a}$`")

      val expected =
        SoftAST.StringInterpolation(
          index = indexOf(">>`${a}$`<<"),
          startTick = Tick(">>`<<${a}$`"),
          interpolation = Seq(
            Right(
              Seq(
                SoftAST.InterpolatedBlock(
                  index = indexOf("`>>${a}<<$`"),
                  dollar = Dollar("`>>$<<{a}$`").code,
                  block = SoftAST.Block(
                    index = indexOf("`$>>{a}<<$`"),
                    openCurly = OpenCurly("`$>>{<<a}$`"),
                    parts = Seq(Identifier("`${>>a<<}$`")),
                    closeCurly = CloseCurly("`${a>>}<<$`")
                  )
                ),
                SoftAST.InterpolatedBlock(
                  index = indexOf("`${a}>>$<<`"),
                  dollar = Dollar("`${a}>>$<<`").code,
                  block = SoftAST.BlockExpected(indexOf("`${a}$>><<`"))
                )
              )
            )
          ),
          endTick = Tick("`${a}$>>`<<").code
        )

      ast shouldBe expected
    }
  }

  "interpolated string" when {
    "$" in {
      val ast = parseStringInterpolation("`${a}`")

      val expected =
        SoftAST.StringInterpolation(
          index = indexOf(">>`${a}`<<"),
          startTick = Tick(">>`<<${a}`"),
          interpolation = Seq(
            Right(
              Seq(
                SoftAST.InterpolatedBlock(
                  index = indexOf("`>>${a}<<`"),
                  dollar = Dollar("`>>$<<{a}`").code,
                  block = SoftAST.Block(
                    index = indexOf("`$>>{a}<<`"),
                    openCurly = OpenCurly("`$>>{<<a}`"),
                    parts = Seq(Identifier("`${>>a<<}`")),
                    closeCurly = CloseCurly("`${a>>}<<`")
                  )
                )
              )
            )
          ),
          endTick = Tick("`${a}>>`<<").code
        )

      ast shouldBe expected
    }

    "$$$ (partially escaped)" in {
      val ast = parseStringInterpolation("`$$${a}`")

      val expected =
        SoftAST.StringInterpolation(
          index = indexOf(">>`$$${a}`<<"),
          startTick = Tick(">>`<<$$${a}`"),
          interpolation = Seq(
            Left(
              Seq(
                Dollar("`>>$<<$$${a}`").code,
                Dollar("`$>>$<<$${a}`").code
              )
            ),
            Right(
              Seq(
                SoftAST.InterpolatedBlock(
                  index = indexOf("`$$>>${a}<<`"),
                  dollar = Dollar("`$$>>$<<{a}`").code,
                  block = SoftAST.Block(
                    index = indexOf("`$$$>>{a}<<`"),
                    openCurly = OpenCurly("`$$$>>{<<a}`"),
                    parts = Seq(Identifier("`$$${>>a<<}`")),
                    closeCurly = CloseCurly("`$$${a>>}<<`")
                  )
                )
              )
            )
          ),
          endTick = Tick("`$$${a}>>`<<").code
        )

      ast shouldBe expected
    }

    "text & interpolation" in {
      val ast = parseStringInterpolation("`a ${b} $${c} d ${e}${f}`")

      val expected =
        SoftAST.StringInterpolation(
          index = indexOf(">>`a ${b} $${c} d ${e}${f}`<<"),
          startTick = Tick(">>`<<a ${b} $${c} d ${e}${f}`"),
          interpolation = Seq(
            Left(
              Seq(
                CodeString("`>>a <<${b} $${c} d ${e}${f}`")
              )
            ),
            Right(
              Seq(
                SoftAST.InterpolatedBlock(
                  index = indexOf("`a >>${b}<< $${c} d ${e}${f}`"),
                  dollar = Dollar("`a >>$<<{b} $${c} d ${e}${f}`").code,
                  block = SoftAST.Block(
                    index = indexOf("`a $>>{b}<< $${c} d ${e}${f}`"),
                    openCurly = OpenCurly("`a $>>{<<b} $${c} d ${e}${f}`"),
                    parts = Seq(Identifier("`a ${>>b<<} $${c} d ${e}${f}`")),
                    closeCurly = CloseCurly("`a ${b>>}<< $${c} d ${e}${f}`")
                  )
                )
              )
            ),
            Left(
              Seq(
                CodeString("`a ${b}>> <<$${c} d ${e}${f}`"),
                Dollar("`a ${b} >>$<<${c} d ${e}${f}`").code,
                Dollar("`a ${b} $>>$<<{c} d ${e}${f}`").code,
                CodeString("`a ${b} $$>>{c} d <<${e}${f}`")
              )
            ),
            Right(
              Seq(
                SoftAST.InterpolatedBlock(
                  index = indexOf("`a ${b} $${c} d >>${e}<<${f}`"),
                  dollar = Dollar("`a ${b} $${c} d >>$<<{e}${f}`").code,
                  block = SoftAST.Block(
                    index = indexOf("`a ${b} $${c} d $>>{e}<<${f}`"),
                    openCurly = OpenCurly("`a ${b} $${c} d $>>{<<e}${f}`"),
                    parts = Seq(Identifier("`a ${b} $${c} d ${>>e<<}${f}`")),
                    closeCurly = CloseCurly("`a ${b} $${c} d ${e>>}<<${f}`")
                  )
                ),
                SoftAST.InterpolatedBlock(
                  index = indexOf("`a ${b} $${c} d ${e}>>${f}<<`"),
                  dollar = Dollar("`a ${b} $${c} d ${e}>>$<<{f}`").code,
                  block = SoftAST.Block(
                    index = indexOf("`a ${b} $${c} d ${e}$>>{f}<<`"),
                    openCurly = OpenCurly("`a ${b} $${c} d ${e}$>>{<<f}`"),
                    parts = Seq(Identifier("`a ${b} $${c} d ${e}${>>f<<}`")),
                    closeCurly = CloseCurly("`a ${b} $${c} d ${e}${f>>}<<`")
                  )
                )
              )
            )
          ),
          endTick = Tick("`a ${b} $${c} d ${e}${f}>>`<<").code
        )

      ast shouldBe expected
    }
  }

}
