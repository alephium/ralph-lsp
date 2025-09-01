// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.compiler.parser.soft

import org.alephium.ralph.lsp.access.compiler.parser.soft.TestParser._
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.{SoftAST, Token}
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.TestSoftAST._
import org.alephium.ralph.lsp.access.util.TestCodeUtil._
import org.alephium.ralph.lsp.access.util.TestFastParse.assertIsFastParseError
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class StructDeconstructorParserSpec extends AnyWordSpec {

  "fail" when {
    "identifier only" in {
      // Identifier on its own is not a deconstructor
      assertIsFastParseError {
        parseStructDeconstructorNoCodeCheck("MyStruct")
      }
    }

    "identifier followed by space" in {
      // Identifier on its own is not a deconstructor
      assertIsFastParseError {
        parseStructDeconstructorNoCodeCheck("MyStruct ")
      }
    }

    "block only" in {
      // A block on its own is not a deconstructor
      assertIsFastParseError {
        parseStructDeconstructorNoCodeCheck("{}")
      }
    }
  }

  "identifier followed by open curly" should {
    "parse as a deconstructor" in {
      val ast = parseStructDeconstructor("MyStruct {")

      ast shouldBe
        SoftAST.StructDeconstructor(
          index = indexOf(">>MyStruct {<<"),
          identifier = Identifier(">>MyStruct<< {"),
          preParamSpace = Some(Space("MyStruct>> <<{")),
          params = SoftAST.Group(
            index = indexOf("MyStruct >>{<<"),
            openToken = Some(OpenCurly("MyStruct >>{<<")),
            preHeadExpressionSpace = None,
            headExpression = None,
            preTailExpressionSpace = None,
            tailExpressions = Seq.empty,
            closeToken = Some(TokenExpected("MyStruct {>><<", Token.CloseCurly))
          )
        )
    }
  }

  "identifier followed by an empty block" should {
    "parse as a deconstructor" in {
      val ast = parseStructDeconstructor("MyStruct {}")

      ast shouldBe
        SoftAST.StructDeconstructor(
          index = indexOf(">>MyStruct {}<<"),
          identifier = Identifier(">>MyStruct<< {}"),
          preParamSpace = Some(Space("MyStruct>> <<{}")),
          params = SoftAST.Group(
            index = indexOf("MyStruct >>{}<<"),
            openToken = Some(OpenCurly("MyStruct >>{<<}")),
            preHeadExpressionSpace = None,
            headExpression = None,
            preTailExpressionSpace = None,
            tailExpressions = Seq.empty,
            closeToken = Some(CloseCurly("MyStruct {>>}<<"))
          )
        )
    }
  }

  "field with complex type params" should {
    "parse as a deconstructor" in {
      val ast = parseStructDeconstructor("MyStruct {array: arrayCopy, string: stringCopy}")

      ast shouldBe
        SoftAST.StructDeconstructor(
          index = indexOf(">>MyStruct {array: arrayCopy, string: stringCopy}<<"),
          identifier = Identifier(">>MyStruct<< {array: arrayCopy, string: stringCopy}"),
          preParamSpace = Some(Space("MyStruct>> <<{array: arrayCopy, string: stringCopy}")),
          params = SoftAST.Group(
            index = indexOf("MyStruct >>{array: arrayCopy, string: stringCopy}<<"),
            openToken = Some(OpenCurly("MyStruct >>{<<array: arrayCopy, string: stringCopy}")),
            preHeadExpressionSpace = None,
            headExpression = Some(
              SoftAST.StructDeconstructorField(
                index = indexOf("MyStruct {>>array: arrayCopy<<, string: stringCopy}"),
                expressionLeft = Identifier("MyStruct {>>array<<: arrayCopy, string: stringCopy}"),
                reference = Some(
                  SoftAST.StructDeconstructorFieldReference(
                    index = indexOf("MyStruct {array>>: arrayCopy<<, string: stringCopy}"),
                    preColonSpace = None,
                    colon = Colon("MyStruct {array>>:<< arrayCopy, string: stringCopy}"),
                    preExpressionSpace = Some(Space("MyStruct {array:>> <<arrayCopy, string: stringCopy}")),
                    expressionRight = Identifier("MyStruct {array: >>arrayCopy<<, string: stringCopy}")
                  )
                )
              )
            ),
            preTailExpressionSpace = None,
            tailExpressions = Seq(
              SoftAST.GroupTail(
                index = indexOf("MyStruct {array: arrayCopy>>, string: stringCopy<<}"),
                delimiter = Comma("MyStruct {array: arrayCopy>>,<< string: stringCopy}"),
                preExpressionSpace = Some(Space("MyStruct {array: arrayCopy,>> <<string: stringCopy}")),
                expression = SoftAST.StructDeconstructorField(
                  index = indexOf("MyStruct {array: arrayCopy, >>string: stringCopy<<}"),
                  expressionLeft = Identifier("MyStruct {array: arrayCopy, >>string<<: stringCopy}"),
                  reference = Some(
                    SoftAST.StructDeconstructorFieldReference(
                      index = indexOf("MyStruct {array: arrayCopy, string>>: stringCopy<<}"),
                      preColonSpace = None,
                      colon = Colon("MyStruct {array: arrayCopy, string>>:<< stringCopy}"),
                      preExpressionSpace = Some(Space("MyStruct {array: arrayCopy, string:>> <<stringCopy}")),
                      expressionRight = Identifier("MyStruct {array: arrayCopy, string: >>stringCopy<<}")
                    )
                  )
                ),
                postExpressionSpace = None
              )
            ),
            closeToken = Some(CloseCurly("MyStruct {array: arrayCopy, string: stringCopy>>}<<"))
          )
        )
    }
  }

  "parse nested struct deconstructors" in {
    val ast = parseStructDeconstructor("Customer { id: idCopy, info: Info { phone: phoneCopy }}")

    ast shouldBe
      SoftAST.StructDeconstructor(
        index = indexOf(">>Customer { id: idCopy, info: Info { phone: phoneCopy }}<<"),
        identifier = Identifier(">>Customer<< { id: idCopy, info: Info { phone: phoneCopy }}"),
        preParamSpace = Some(Space("Customer>> <<{ id: idCopy, info: Info { phone: phoneCopy }}")),
        params = SoftAST.Group(
          index = indexOf("Customer >>{ id: idCopy, info: Info { phone: phoneCopy }}<<"),
          openToken = Some(OpenCurly("Customer >>{<< id: idCopy, info: Info { phone: phoneCopy }}")),
          preHeadExpressionSpace = Some(Space("Customer {>> <<id: idCopy, info: Info { phone: phoneCopy }}")),
          headExpression = Some(
            SoftAST.StructDeconstructorField(
              index = indexOf("Customer { >>id: idCopy<<, info: Info { phone: phoneCopy }}"),
              expressionLeft = Identifier("Customer { >>id<<: idCopy, info: Info { phone: phoneCopy }}"),
              reference = Some(
                SoftAST.StructDeconstructorFieldReference(
                  index = indexOf("Customer { id>>: idCopy<<, info: Info { phone: phoneCopy }}"),
                  preColonSpace = None,
                  colon = Colon("Customer { id>>:<< idCopy, info: Info { phone: phoneCopy }}"),
                  preExpressionSpace = Some(Space("Customer { id:>> <<idCopy, info: Info { phone: phoneCopy }}")),
                  expressionRight = Identifier("Customer { id: >>idCopy<<, info: Info { phone: phoneCopy }}")
                )
              )
            )
          ),
          preTailExpressionSpace = None,
          tailExpressions = Seq(
            SoftAST.GroupTail(
              index = indexOf("Customer { id: idCopy>>, info: Info { phone: phoneCopy }<<}"),
              delimiter = Comma("Customer { id: idCopy>>,<< info: Info { phone: phoneCopy }}"),
              preExpressionSpace = Some(Space("Customer { id: idCopy,>> <<info: Info { phone: phoneCopy }}")),
              expression = SoftAST.StructDeconstructorField(
                index = indexOf("Customer { id: idCopy, >>info: Info { phone: phoneCopy }<<}"),
                expressionLeft = Identifier("Customer { id: idCopy, >>info<<: Info { phone: phoneCopy }}"),
                reference = Some(
                  SoftAST.StructDeconstructorFieldReference(
                    index = indexOf("Customer { id: idCopy, info>>: Info { phone: phoneCopy }<<}"),
                    preColonSpace = None,
                    colon = Colon("Customer { id: idCopy, info>>:<< Info { phone: phoneCopy }}"),
                    preExpressionSpace = Some(Space("Customer { id: idCopy, info:>> <<Info { phone: phoneCopy }}")),
                    expressionRight = SoftAST.StructDeconstructor(
                      index = indexOf("Customer { id: idCopy, info: >>Info { phone: phoneCopy }<<}"),
                      identifier = Identifier("Customer { id: idCopy, info: >>Info<< { phone: phoneCopy }}"),
                      preParamSpace = Some(Space("Customer { id: idCopy, info: Info>> <<{ phone: phoneCopy }}")),
                      params = SoftAST.Group(
                        index = indexOf("Customer { id: idCopy, info: Info >>{ phone: phoneCopy }<<}"),
                        openToken = Some(OpenCurly("Customer { id: idCopy, info: Info >>{<< phone: phoneCopy }}")),
                        preHeadExpressionSpace = Some(Space("Customer { id: idCopy, info: Info {>> <<phone: phoneCopy }}")),
                        headExpression = Some(
                          SoftAST.StructDeconstructorField(
                            index = indexOf("Customer { id: idCopy, info: Info { >>phone: phoneCopy<< }}"),
                            expressionLeft = Identifier("Customer { id: idCopy, info: Info { >>phone<<: phoneCopy }}"),
                            reference = Some(
                              SoftAST.StructDeconstructorFieldReference(
                                index = indexOf("Customer { id: idCopy, info: Info { phone>>: phoneCopy<< }}"),
                                preColonSpace = None,
                                colon = Colon("Customer { id: idCopy, info: Info { phone>>:<< phoneCopy }}"),
                                preExpressionSpace = Some(Space("Customer { id: idCopy, info: Info { phone:>> <<phoneCopy }}")),
                                expressionRight = Identifier("Customer { id: idCopy, info: Info { phone: >>phoneCopy<< }}")
                              )
                            )
                          )
                        ),
                        preTailExpressionSpace = Some(Space("Customer { id: idCopy, info: Info { phone: phoneCopy>> <<}}")),
                        tailExpressions = Seq.empty,
                        closeToken = Some(CloseCurly("Customer { id: idCopy, info: Info { phone: phoneCopy >>}<<}"))
                      )
                    )
                  )
                )
              ),
              postExpressionSpace = None
            )
          ),
          closeToken = Some(CloseCurly("Customer { id: idCopy, info: Info { phone: phoneCopy }>>}<<"))
        )
      )
  }

  "deconstructor is referenced" when {
    "immutable" in {
      val ast = parseStructDeconstructor("MyStruct {number: numberCopy}")

      ast shouldBe
        SoftAST.StructDeconstructor(
          index = indexOf(">>MyStruct {number: numberCopy}<<"),
          identifier = Identifier(">>MyStruct<< {number: numberCopy}"),
          preParamSpace = Some(Space("MyStruct>> <<{number: numberCopy}")),
          params = SoftAST.Group(
            index = indexOf("MyStruct >>{number: numberCopy}<<"),
            openToken = Some(OpenCurly("MyStruct >>{<<number: numberCopy}")),
            preHeadExpressionSpace = None,
            headExpression = Some(
              SoftAST.StructDeconstructorField(
                index = indexOf("MyStruct {>>number: numberCopy<<}"),
                expressionLeft = Identifier("MyStruct {>>number<<: numberCopy}"),
                reference = Some(
                  SoftAST.StructDeconstructorFieldReference(
                    index = indexOf("MyStruct {number>>: numberCopy<<}"),
                    preColonSpace = None,
                    colon = Colon("MyStruct {number>>:<< numberCopy}"),
                    preExpressionSpace = Some(Space("MyStruct {number:>> <<numberCopy}")),
                    expressionRight = Identifier("MyStruct {number: >>numberCopy<<}")
                  )
                )
              )
            ),
            preTailExpressionSpace = None,
            tailExpressions = Seq.empty,
            closeToken = Some(CloseCurly("MyStruct {number: numberCopy>>}<<"))
          )
        )
    }

    "mutable" in {
      val ast = parseStructDeconstructor("MyStruct {number: mut numberCopy}")

      ast shouldBe
        SoftAST.StructDeconstructor(
          index = indexOf(">>MyStruct {number: mut numberCopy}<<"),
          identifier = Identifier(">>MyStruct<< {number: mut numberCopy}"),
          preParamSpace = Some(Space("MyStruct>> <<{number: mut numberCopy}")),
          params = SoftAST.Group(
            index = indexOf("MyStruct >>{number: mut numberCopy}<<"),
            openToken = Some(OpenCurly("MyStruct >>{<<number: mut numberCopy}")),
            preHeadExpressionSpace = None,
            headExpression = Some(
              SoftAST.StructDeconstructorField(
                index = indexOf("MyStruct {>>number: mut numberCopy<<}"),
                expressionLeft = Identifier("MyStruct {>>number<<: mut numberCopy}"),
                reference = Some(
                  SoftAST.StructDeconstructorFieldReference(
                    index = indexOf("MyStruct {number>>: mut numberCopy<<}"),
                    preColonSpace = None,
                    colon = Colon("MyStruct {number>>:<< mut numberCopy}"),
                    preExpressionSpace = Some(Space("MyStruct {number:>> <<mut numberCopy}")),
                    expressionRight = SoftAST.MutableBinding(
                      index = indexOf("MyStruct {number: >>mut numberCopy<<}"),
                      mut = Mut("MyStruct {number: >>mut<< numberCopy}"),
                      space = Some(Space("MyStruct {number: mut>> <<numberCopy}")),
                      identifier = Identifier("MyStruct {number: mut >>numberCopy<<}")
                    )
                  )
                )
              )
            ),
            preTailExpressionSpace = None,
            tailExpressions = Seq.empty,
            closeToken = Some(CloseCurly("MyStruct {number: mut numberCopy>>}<<"))
          )
        )
    }
  }

  "deconstructor is not rereferenced" when {
    "single value param" when {
      "immutable param" in {
        val ast = parseStructDeconstructor("MyStruct{ name }")

        ast shouldBe
          SoftAST.StructDeconstructor(
            index = indexOf(">>MyStruct{ name }<<"),
            identifier = Identifier(">>MyStruct<<{ name }"),
            preParamSpace = None,
            params = SoftAST.Group(
              index = indexOf("MyStruct>>{ name }<<"),
              openToken = Some(OpenCurly("MyStruct>>{<< name }")),
              preHeadExpressionSpace = Some(Space("MyStruct{>> <<name }")),
              headExpression = Some(
                SoftAST.StructDeconstructorField(
                  index = indexOf("MyStruct{ >>name<< }"),
                  expressionLeft = Identifier("MyStruct{ >>name<< }"),
                  reference = None
                )
              ),
              preTailExpressionSpace = Some(Space("MyStruct{ name>> <<}")),
              tailExpressions = Seq.empty,
              closeToken = Some(CloseCurly("MyStruct{ name >>}<<"))
            )
          )
      }

      "mutable param" in {
        val ast = parseStructDeconstructor("MyStruct{ mut name }")

        ast shouldBe
          SoftAST.StructDeconstructor(
            index = indexOf(">>MyStruct{ mut name }<<"),
            identifier = Identifier(">>MyStruct<<{ mut name }"),
            preParamSpace = None,
            params = SoftAST.Group(
              index = indexOf("MyStruct>>{ mut name }<<"),
              openToken = Some(OpenCurly("MyStruct>>{<< mut name }")),
              preHeadExpressionSpace = Some(Space("MyStruct{>> <<mut name }")),
              headExpression = Some(
                SoftAST.StructDeconstructorField(
                  index = indexOf("MyStruct{ >>mut name<< }"),
                  expressionLeft = SoftAST.MutableBinding(
                    index = indexOf("MyStruct{ >>mut name<< }"),
                    mut = Mut("MyStruct{ >>mut<< name }"),
                    space = Some(Space("MyStruct{ mut>> <<name }")),
                    identifier = Identifier("MyStruct{ mut >>name<< }")
                  ),
                  reference = None
                )
              ),
              preTailExpressionSpace = Some(Space("MyStruct{ mut name>> <<}")),
              tailExpressions = Seq.empty,
              closeToken = Some(CloseCurly("MyStruct{ mut name >>}<<"))
            )
          )
      }
    }

    "3 value params" in {
      // `one` and `three` are stored as unresolved
      // `two` is stored with its right-hand-side stored as expression-expected
      val ast = parseStructDeconstructor("MyStruct{ one, two:, three }")

      ast shouldBe
        SoftAST.StructDeconstructor(
          index = indexOf(">>MyStruct{ one, two:, three }<<"),
          identifier = Identifier(">>MyStruct<<{ one, two:, three }"),
          preParamSpace = None,
          params = SoftAST.Group(
            index = indexOf("MyStruct>>{ one, two:, three }<<"),
            openToken = Some(OpenCurly("MyStruct>>{<< one, two:, three }")),
            preHeadExpressionSpace = Some(Space("MyStruct{>> <<one, two:, three }")),
            headExpression = Some(
              SoftAST.StructDeconstructorField(
                index = indexOf("MyStruct{ >>one<<, two:, three }"),
                expressionLeft = Identifier("MyStruct{ >>one<<, two:, three }"),
                reference = None
              )
            ),
            preTailExpressionSpace = None,
            tailExpressions = Seq(
              SoftAST.GroupTail(
                index = indexOf("MyStruct{ one>>, two:<<, three }"),
                delimiter = Comma("MyStruct{ one>>,<< two:, three }"),
                preExpressionSpace = Some(Space("MyStruct{ one,>> <<two:, three }")),
                expression = SoftAST.StructDeconstructorField(
                  index = indexOf("MyStruct{ one, >>two:<<, three }"),
                  expressionLeft = Identifier("MyStruct{ one, >>two<<:, three }"),
                  reference = Some(
                    SoftAST.StructDeconstructorFieldReference(
                      index = indexOf("MyStruct{ one, two>>:<<, three }"),
                      preColonSpace = None,
                      colon = Colon("MyStruct{ one, two>>:<<, three }"),
                      preExpressionSpace = None,
                      expressionRight = ExpressionExpected("MyStruct{ one, two:>><<, three }")
                    )
                  )
                ),
                postExpressionSpace = None
              ),
              SoftAST.GroupTail(
                index = indexOf("MyStruct{ one, two:>>, three <<}"),
                delimiter = Comma("MyStruct{ one, two:>>,<< three }"),
                preExpressionSpace = Some(Space("MyStruct{ one, two:,>> <<three }")),
                expression = SoftAST.StructDeconstructorField(
                  index = indexOf("MyStruct{ one, two:, >>three<< }"),
                  expressionLeft = Identifier("MyStruct{ one, two:, >>three<< }"),
                  reference = None
                ),
                postExpressionSpace = Some(Space("MyStruct{ one, two:, three>> <<}"))
              )
            ),
            closeToken = Some(CloseCurly("MyStruct{ one, two:, three >>}<<"))
          )
        )
    }
  }

  "unresolved" when {
    "invalid single token parameter" in {
      val ast = parseStructDeconstructor("MyStruct{ ⚠️ }")

      ast shouldBe
        SoftAST.StructDeconstructor(
          index = indexOf(">>MyStruct{ ⚠️ }<<"),
          identifier = Identifier(">>MyStruct<<{ ⚠️ }"),
          preParamSpace = None,
          params = SoftAST.Group(
            index = indexOf("MyStruct>>{ ⚠️ }<<"),
            openToken = Some(OpenCurly("MyStruct>>{<< ⚠️ }")),
            preHeadExpressionSpace = Some(Space("MyStruct{>> <<⚠️ }")),
            headExpression = Some(Unresolved("MyStruct{ >>⚠️<< }")),
            preTailExpressionSpace = Some(Space("MyStruct{ ⚠️>> <<}")),
            tailExpressions = Seq.empty,
            closeToken = Some(CloseCurly("MyStruct{ ⚠️ >>}<<"))
          )
        )
    }

    "invalid single double parameter" in {
      val ast = parseStructDeconstructor("MyStruct{ ⚠️⚠️ }")

      ast shouldBe
        SoftAST.StructDeconstructor(
          index = indexOf(">>MyStruct{ ⚠️⚠️ }<<"),
          identifier = Identifier(">>MyStruct<<{ ⚠️⚠️ }"),
          preParamSpace = None,
          params = SoftAST.Group(
            index = indexOf("MyStruct>>{ ⚠️⚠️ }<<"),
            openToken = Some(OpenCurly("MyStruct>>{<< ⚠️⚠️ }")),
            preHeadExpressionSpace = Some(Space("MyStruct{>> <<⚠️⚠️ }")),
            headExpression = Some(Unresolved("MyStruct{ >>⚠️⚠️<< }")),
            preTailExpressionSpace = Some(Space("MyStruct{ ⚠️⚠️>> <<}")),
            tailExpressions = Seq.empty,
            closeToken = Some(CloseCurly("MyStruct{ ⚠️⚠️ >>}<<"))
          )
        )
    }
  }

}
