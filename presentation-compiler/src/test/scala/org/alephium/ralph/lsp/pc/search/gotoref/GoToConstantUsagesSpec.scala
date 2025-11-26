// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GoToConstantUsagesSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "constant has no usage" when {
      "local" when {
        "strict parseable" in {
          goToReferences() {
            """
              |Contract GoToConstant() {
              |
              |  const MyCons@@tant = 0
              |
              |  pub fn function() -> () {
              |
              |  }
              |}
              |""".stripMargin
          }
        }

        "soft parseable" in {
          goToReferencesSoft() {
            """
              |Contract GoToConstant {
              |
              |  const MyCons@@tant = 0
              |
              |  fn function { }
              |}
              |""".stripMargin
          }
        }
      }

      "global" when {
        "strict parseable" in {
          goToReferences() {
            """
              |const MyCons@@tant = 0
              |
              |Contract GoToConstant() {
              |
              |  pub fn function() -> () {
              |
              |  }
              |}
              |""".stripMargin
          }
        }

        "soft parseable" in {
          goToReferencesSoft() {
            """
              |const MyCons@@tant =
              |
              |Contract GoToConstant {
              |
              |  pub fn function { }
              |}
              |""".stripMargin
          }
        }
      }
    }
  }

  "return non-empty" when {
    "constant has multiple usages" when {
      "local constants" when {
        "strict parseable" when {
          def doTest(contractName: String) =
            goToReferencesForAll(">>MyConstant<<".r, ">>MyCons@@tant<<")(
              s"""
                 |Contract $contractName() {
                 |
                 |  const MyCons@@tant = 0
                 |  const MyConstant_B = 1
                 |
                 |  pub fn function() -> [U256; >>MyConstant<<] {
                 |    let my_constant = >>MyConstant<<
                 |    let my_constant2 = >>MyConstant<<
                 |    let my_constant3 = MyConstant_B
                 |    for (let mut index = 0; index <= 4; index = index + 1) {
                 |      let my_constant4 = >>MyConstant<<
                 |      let my_constant5 = MyConstant_B
                 |    }
                 |    return [0; >>MyConstant<<]
                 |  }
                 |}
                 |""".stripMargin
            )

          "constant and contract have the same ID" in {
            // the constant name is also "MyConstant"
            doTest(contractName = "MyConstant")
          }

          "constant and contract have unique IDs" in {
            doTest(contractName = "MyContract")
          }
        }

        "soft parseable" when {
          def doTest(contractName: String) =
            goToReferencesSoftForAll(">>MyConstant<<".r, ">>MyCons@@tant<<")(
              s"""
                 |Contract $contractName {
                 |
                 |  const MyCons@@tant = 0
                 |  const MyConstant_B = 1
                 |
                 |  [U256; >>MyConstant<<]
                 |
                 |  pub function -> [U256; >>MyConstant<<] {
                 |    let my_constant = >>MyConstant<<
                 |    let my_constant2 = >>MyConstant<<
                 |    let my_constant3 = MyConstant_B
                 |    for (let mut index = 0; index <= 4; index = index + 1) {
                 |      let my_constant4 = >>MyConstant<<
                 |      let my_constant5 = MyConstant_B
                 |    }
                 |    return [0; >>MyConstant<<]
                 |  }
                 |  
                 |  let my_constant = >>MyConstant<<
                 |  let my_constant2 = >>MyConstant<<
                 |  let my_constant3 = MyConstant_B
                 |  >>MyConstant<<
                 |  for (let mut index = 0; index <= 4; index = index + 1) {
                 |    let my_constant4 = >>MyConstant<<
                 |    let my_constant5 = MyConstant_B
                 |    >>MyConstant<<
                 |  }
                 |  >>MyConstant<<
                 |  return [0; >>MyConstant<<]
                 |  >>MyConstant<<
                 |}
                 |""".stripMargin
            )

          "constant and contract have the same ID" in {
            // the constant name is also "MyConstant"
            doTest(contractName = "MyConstant")
          }

          "constant and contract have unique IDs" in {
            doTest(contractName = "MyContract")
          }
        }
      }

      "global constants" when {
        "strict parseable" when {
          def doTest(contractName: String) =
            goToReferencesStrict() {
              s"""
                 |const MyCons@@tant = 0
                 |const MyConstant_B = 1
                 |
                 |Contract $contractName() {
                 |
                 |  pub fn function() -> [U256; >>MyConstant<<] {
                 |    let my_constant = >>MyConstant<<
                 |    let my_constant2 = >>MyConstant<<
                 |    let my_constant3 = MyConstant_B
                 |    for (let mut index = 0; index <= 4; index = index + 1) {
                 |      let my_constant4 = >>MyConstant<<
                 |      let my_constant5 = MyConstant_B
                 |    }
                 |    return [0; >>MyConstant<<]
                 |  }
                 |}
                 |""".stripMargin
            }

          "constant and contract have the same ID" in {
            // the constant name is also "MyConstant"
            doTest(contractName = "MyConstant")
          }

          "constant and contract have unique IDs" in {
            doTest(contractName = "MyContract")
          }
        }

        "soft parseable" when {
          def doTest(contractName: String) =
            goToReferencesSoft() {
              s"""
                 |const MyCons@@tant = 0
                 |const MyConstant_B = 1
                 |
                 |Contract $contractName {
                 |
                 |  >>MyConstant<<
                 |  {
                 |    [0; >>MyConstant<<]
                 |    >>MyConstant<<
                 |  }
                 |
                 |  fn function -> [U256; >>MyConstant<<] {
                 |    let my_constant = >>MyConstant<<
                 |    let my_constant2 = >>MyConstant<<
                 |    let my_constant3 = MyConstant_B
                 |    for (let mut index = 0; index <= 4; index = index + 1) {
                 |      let my_constant4 = >>MyConstant<<
                 |      let my_constant5 = MyConstant_B
                 |    }
                 |    return [0; >>MyConstant<<]
                 |  }
                 |
                 |  let my_constant = >>MyConstant<<
                 |  let my_constant2 = >>MyConstant<<
                 |  let my_constant3 = MyConstant_B
                 |  for (let mut index = 0; index <= 4; index = index + 1) {
                 |    let my_constant4 = >>MyConstant<<
                 |    let my_constant5 = MyConstant_B
                 |  }
                 |  [0; >>MyConstant<<]
                 |  return [0; >>MyConstant<<]
                 |}
                 |""".stripMargin
            }

          "constant and contract have the same ID" in {
            // the constant name is also "MyConstant"
            doTest(contractName = "MyConstant")
          }

          "constant and contract have unique IDs" in {
            doTest(contractName = "MyContract")
          }
        }
      }
    }

    "there is inheritance" when {
      "local constant" when {
        "strict parseable" in {
          goToReferencesForAll(">>MyConstant<<".r, ">>MyCons@@tant<<")(
            """
              |Abstract Contract Parent() {
              |
              |  const MyCons@@tant = 0
              |
              |  fn function0() -> [U256; >>MyConstant<<] {
              |    let my_constant2 = >>MyConstant<<
              |    let my_constant3 = MyConstant_B
              |    return [0; >>MyConstant<<]
              |  }
              |}
              |
              |Contract Parent1() extends Parent() {
              |
              |  pub fn function1() -> [U256; >>MyConstant<<] {
              |    let my_constant2 = >>MyConstant<<
              |    let my_constant3 = MyConstant_B
              |    return [0; >>MyConstant<<]
              |  }
              |}
              |
              |Contract Child() extends Parent1() {
              |
              |  pub fn function2() -> [U256; >>MyConstant<<] {
              |    let my_constant2 = >>MyConstant<<
              |    let my_constant3 = MyConstant_B
              |    return [0; >>MyConstant<<]
              |  }
              |}
              |""".stripMargin
          )
        }

        "soft parseable" in {
          goToReferencesSoftForAll(">>MyConstant<<".r, ">>MyCons@@tant<<")(
            """
              |Abstract Contract Parent {
              |
              |  const MyCons@@tant = 0
              |
              |  fn function0 -> [U256; >>MyConstant<<] {
              |    let my_constant2 = >>MyConstant<<
              |    let my_constant3 = MyConstant_B
              |    >>MyConstant<<
              |    return [0; >>MyConstant<<]
              |  }
              |
              |  let my_constant2 = >>MyConstant<<
              |  let my_constant3 = MyConstant_B
              |  >>MyConstant<<
              |  return [0; >>MyConstant<<]
              |}
              |
              |Contract Parent1 extends Parent {
              |
              |  pub fn function1 -> [U256; >>MyConstant<<] {
              |    let my_constant2 = >>MyConstant<<
              |    let my_constant3 = MyConstant_B
              |    >>MyConstant<<
              |    return [0; >>MyConstant<<]
              |  }
              |
              |  let my_constant2 = >>MyConstant<<
              |  let my_constant3 = MyConstant_B
              |  >>MyConstant<<
              |  return [0; >>MyConstant<<]
              |}
              |
              |Contract Child extends Parent1 {
              |
              |  fn function2 -> [U256; >>MyConstant<<] {
              |    let my_constant2 = >>MyConstant<<
              |    let my_constant3 = MyConstant_B
              |    >>MyConstant<<
              |    return [0; >>MyConstant<<]
              |  }
              |
              |  let my_constant2 = >>MyConstant<<
              |  let my_constant3 = MyConstant_B
              |  >>MyConstant<<
              |  return [0; >>MyConstant<<]
              |}
              |""".stripMargin
          )
        }
      }

      "global constant is defined" when {
        "strict parseable" when {
          def doTest(
              before: String,
              after: String) =
            goToReferencesForAll(">>MyConstant<<".r, ">>MyCons@@tant<<")(
              s"""
                 |$before
                 |
                 |Abstract Contract Parent() {
                 |
                 |  fn function0() -> [U256; >>MyConstant<<] {
                 |    let my_constant2 = >>MyConstant<<
                 |    let my_constant3 = MyConstant_B
                 |    return [0; >>MyConstant<<]
                 |  }
                 |}
                 |
                 |Contract Parent1() extends Parent() {
                 |
                 |  pub fn function1() -> [U256; >>MyConstant<<] {
                 |    let my_constant2 = >>MyConstant<<
                 |    let my_constant3 = MyConstant_B
                 |    return [0; >>MyConstant<<]
                 |  }
                 |}
                 |
                 |Contract Child() extends Parent1() {
                 |
                 |  pub fn function2() -> [U256; >>MyConstant<<] {
                 |    let my_constant2 = >>MyConstant<<
                 |    let my_constant3 = MyConstant_B
                 |    return [0; >>MyConstant<<]
                 |  }
                 |}
                 |
                 |$after
                 |""".stripMargin
            )

          "before its usage" in {
            doTest(
              before = "const MyCons@@tant = 0",
              after = "const ANOTHER = 2"
            )
          }

          "after its usage" in {
            doTest(
              before = "const ANOTHER = 2",
              after = "const MyCons@@tant = 0"
            )
          }
        }

        "soft parseable" when {
          def doTest(
              before: String,
              after: String) =
            goToReferencesSoftForAll(">>MyConstant<<".r, ">>MyCons@@tant<<")(
              s"""
                 |$before
                 |
                 |Abstract Contract Parent {
                 |
                 |  >>MyConstant<<
                 |
                 |  fn function0 -> [U256; >>MyConstant<<] {
                 |    let my_constant2 = >>MyConstant<<
                 |    let my_constant3 = MyConstant_B
                 |    >>MyConstant<<
                 |    return [0; >>MyConstant<<]
                 |  }
                 |}
                 |
                 |Contract Parent1 extends Parent {
                 |
                 |  >>MyConstant<<
                 |
                 |  pub fn function1 -> [U256; >>MyConstant<<] {
                 |    let my_constant2 = >>MyConstant<<
                 |    let my_constant3 = MyConstant_B
                 |    >>MyConstant<<
                 |    return [0; >>MyConstant<<]
                 |  }
                 |}
                 |
                 |Contract Child extends Parent1 {
                 |
                 |  >>MyConstant<<
                 |
                 |  pub fn function2 -> [U256; >>MyConstant<<] {
                 |    let my_constant2 = >>MyConstant<<
                 |    let my_constant3 = MyConstant_B
                 |    >>MyConstant<<
                 |    return [0; >>MyConstant<<]
                 |  }
                 |
                 |  let my_constant2 = >>MyConstant<<
                 |  let my_constant3 = MyConstant_B
                 |  >>MyConstant<<
                 |  return [0; >>MyConstant<<]
                 |}
                 |
                 |$after
                 |""".stripMargin
            )

          "before its usage" in {
            doTest(
              before = "const MyCons@@tant = 0",
              after = "const ANOTHER = 2"
            )
          }

          "after its usage" in {
            doTest(
              before = "const ANOTHER = 2",
              after = "const MyCons@@tant = 0"
            )
          }
        }
      }
    }

    "constant is used within array type definition" when {
      "global constant" when {
        "strict parseable" in {
          goToReferencesForAll(">>SIZE<<".r, ">>SIZE@@<<")(
            """
              |const SIZE@@ = 2
              |
              |Contract Test() {
              |  fn test() -> [U256; >>SIZE<<] {
              |    return [0; >>SIZE<<]
              |  }
              |}
              |""".stripMargin
          )
        }

        "soft parseable" in {
          goToReferencesSoftForAll(">>SIZE<<".r, ">>SIZE@@<<")(
            """
              |const SIZE@@ = 2
              |
              |Contract Test {
              |  fn test() -> [U256; >>SIZE<<] {
              |    return [0; >>SIZE<<]
              |  }
              |}
              |""".stripMargin
          )
        }
      }

      "local constant" when {
        "strict parseable" in {
          goToReferencesForAll(">>SIZE<<".r, ">>SIZE@@<<")(
            """
              |Contract Test() {
              |
              |  const SIZE@@ = 2
              |
              |  fn test() -> [U256; >>SIZE<<] {
              |    return [0; >>SIZE<<]
              |  }
              |}
              |""".stripMargin
          )
        }

        "soft parseable" in {
          goToReferencesSoftForAll(">>SIZE<<".r, ">>SIZE@@<<")(
            """
              |Contract Test {
              |
              |  const SIZE@@ = 2
              |
              |  fn test -> [U256; >>SIZE<<] {
              |    return [0; >>SIZE<<]
              |  }
              |}
              |""".stripMargin
          )
        }
      }
    }
  }

}
