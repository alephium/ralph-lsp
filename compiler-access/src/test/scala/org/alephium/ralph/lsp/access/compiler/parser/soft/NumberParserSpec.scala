package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NumberParserSpec extends AnyWordSpec with Matchers {

  def assertSimpleNumber(number: String) =
    parseNumber(number) shouldBe
      Number(
        index = indexOf(s">>$number<<"),
        text = number
      )

  "integer" in {
    assertSimpleNumber("10")
    assertSimpleNumber("-10")
    assertSimpleNumber("+10")
  }

  "typed" in {
    assertSimpleNumber("10u")
    assertSimpleNumber("10i")
    assertSimpleNumber("-10u")
    assertSimpleNumber("-10i")
    assertSimpleNumber("+10u")
    assertSimpleNumber("+10i")
  }

  "scientific" in {
    assertSimpleNumber("1e18")
    assertSimpleNumber("-1e18")
    assertSimpleNumber("+1e18")
    assertSimpleNumber("5.12e18")
    assertSimpleNumber("-5.12e18")
    assertSimpleNumber("+5.12e18")
  }

  "underscored" in {
    assertSimpleNumber("1_000_000_000")
    assertSimpleNumber("-1_000_000_000")
    assertSimpleNumber("+1_000_000_000")
  }

  "hex" in {
    assertSimpleNumber("0x12")
    assertSimpleNumber("-0x12")
    assertSimpleNumber("+0x12")
  }

  "unit" when {
    "invalid" in {
      assertSimpleNumber("1alp")
      assertSimpleNumber("-1alp")
      assertSimpleNumber("+1alp")

      assertSimpleNumber("1lph")
      assertSimpleNumber("-1lph")
      assertSimpleNumber("+1lph")

      assertSimpleNumber("1hpla")
      assertSimpleNumber("-1hpla")
      assertSimpleNumber("+1hpla")
    }

    "valid" when {
      "no spaces" when {
        def testWithUnit(numberOnly: String) =
          parseNumber(s"${numberOnly}alph") shouldBe
            SoftAST.Number(
              index = indexOf(s">>${numberOnly}alph<<"),
              documentation = None,
              number = SoftAST.CodeString(
                index = indexOf(s">>$numberOnly<<alph"),
                text = numberOnly
              ),
              unit = Some(
                SoftAST.UnitAlph(
                  index = indexOf(s"$numberOnly>>alph<<"),
                  space = None,
                  unit = AlphLowercase(indexOf(s"$numberOnly>>alph<<"))
                )
              )
            )

        "no sign" in {
          testWithUnit("1")
          testWithUnit("5.12e18")
          testWithUnit("0x12")
        }

        "positive" in {
          testWithUnit("+1")
          testWithUnit("+5.12e18")
          testWithUnit("+0x12")
        }

        "negative" in {
          testWithUnit("-1")
          testWithUnit("-5.12e18")
          testWithUnit("-0x12")
        }
      }

      "with space" when {
        def testWithUnit(numberOnly: String) =
          parseNumber(s"$numberOnly alph") shouldBe
            SoftAST.Number(
              index = indexOf(s">>$numberOnly alph<<"),
              documentation = None,
              number = SoftAST.CodeString(
                index = indexOf(s">>$numberOnly<< alph"),
                text = numberOnly
              ),
              unit = Some(
                SoftAST.UnitAlph(
                  index = indexOf(s"$numberOnly>> alph<<"),
                  space = Some(SpaceOne(indexOf(s"$numberOnly>> <<alph"))),
                  unit = AlphLowercase(indexOf(s"$numberOnly >>alph<<"))
                )
              )
            )

        "no sign" in {
          testWithUnit("1")
          testWithUnit("5.12e18")
          testWithUnit("0x12")
        }

        "positive" in {
          testWithUnit("+1")
          testWithUnit("+5.12e18")
          testWithUnit("+0x12")
        }

        "negative" in {
          testWithUnit("-1")
          testWithUnit("-5.12e18")
          testWithUnit("-0x12")
        }
      }

      "scientific number" when {
        "without space" when {
          "valid unit" in {
            parseNumber("1e-18alph") shouldBe
              SoftAST.Number(
                index = indexOf(s">>1e-18alph<<"),
                documentation = None,
                number = SoftAST.CodeString(
                  index = indexOf(s">>1e-18<<alph"),
                  text = "1e-18"
                ),
                unit = Some(
                  SoftAST.UnitAlph(
                    index = indexOf("1e-18>>alph<<"),
                    space = None,
                    unit = AlphLowercase(indexOf("1e-18>>alph<<"))
                  )
                )
              )
          }

          "invalid unit - 'alp' is typo" in {
            parseNumber("1e-18alp") shouldBe
              SoftAST.Number(
                index = indexOf(s">>1e-18alp<<"),
                documentation = None,
                number = SoftAST.CodeString(
                  index = indexOf(s">>1e-18alp<<"),
                  text = "1e-18alp"
                ),
                unit = None
              )
          }
        }

        "with space" when {
          "valid unit" in {
            parseNumber("1e-18 alph") shouldBe
              SoftAST.Number(
                index = indexOf(s">>1e-18 alph<<"),
                documentation = None,
                number = SoftAST.CodeString(
                  index = indexOf(s">>1e-18<< alph"),
                  text = "1e-18"
                ),
                unit = Some(
                  SoftAST.UnitAlph(
                    index = indexOf("1e-18>> alph<<"),
                    space = Some(SpaceOne(indexOf("1e-18>> <<alph"))),
                    unit = AlphLowercase(indexOf("1e-18 >>alph<<"))
                  )
                )
              )
          }

          "invalid unit - 'alp' is typo" in {
            val body = parseSoft("1e-18 alp")

            val number = body.parts.head.part
            val alp    = body.parts.last.part

            // Note: alp is not a unit. So it's not parsed as part of the number.
            //       Since alp is not part of the number, the number should
            //       not parse the space after 1e-18.
            number shouldBe
              SoftAST.Number(
                index = indexOf(s">>1e-18<< alp"),
                documentation = None,
                number = SoftAST.CodeString(
                  index = indexOf(s">>1e-18<< alp"),
                  text = "1e-18"
                ),
                unit = None
              )

            // alp is stored as an identifier
            alp shouldBe
              Identifier(
                index = indexOf(s"1e-18 >>alp<<"),
                text = "alp"
              )
          }
        }
      }
    }
  }

  "infix numbers with tail negative signs" should {
    "not be parsed as a number, they are infix operations" in {
      // These are not numbers, they are infix operations.
      // The minus sign within the number is for scientific numbers `e-` only.
      Seq(
        "1-1",
        "1- 1"
      ) foreach {
        infixOperation =>
          // it only parses the first number 1, ignoring the sign and the tail number.
          parseNumberNoCodeCheck(infixOperation) shouldBe
            Number(
              index = indexOf(">>1<<"),
              text = "1"
            )

          // parser it from the root SoftParser parses it as an infix operation and not a number.
          val infix = parseSoft(infixOperation)
          infix.parts should have size 1
          infix.parts.head.part shouldBe a[SoftAST.InfixExpression]
      }
    }
  }

  "special cases" in {

    /**
     * See documentation of [[NumberParser.numberOrHex]]
     */
    assertSimpleNumber("1_")
    assertSimpleNumber("1_._")
    assertSimpleNumber("1_.____0")
    assertSimpleNumber("1_._0___0")
  }

}
