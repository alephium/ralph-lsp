// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * Targets test-cases that
 * - execute search on the struct-field,
 * - jumping to its declaration.
 *
 * {{{
 *   struct MyStruct { >>field<<: Type }
 *   let instance = MyStruct { @@field: Value} // constructor
 *   let MyStruct { @@field } = instance       // deconstructor
 * }}}
 */
class GoToStructField_From_StructFieldBindingSpec extends AnyWordSpec with Matchers {

  "return empty" when {
    "a struct field and an assignment value have duplicate names" when {
      "struct is defined external to the contract" in {
        goToDefinition() {
          """
            |struct MyStruct {
            |  structField: Bool
            |}
            |
            |Contract Test() {
            |
            |  pub fn function() -> () {
            |    let copy = struct@@Field
            |  }
            |}
            |""".stripMargin
        }
      }

      "struct is defined within the contract" in {
        goToDefinition() {
          """
            |Contract Test() {
            |
            |  struct MyStruct {
            |    structField: Bool
            |  }
            |
            |  pub fn function() -> () {
            |    let copy = struct@@Field
            |  }
            |}
            |""".stripMargin
        }
      }

      "duplicate struct field name and variable name" when {
        "variable is defined before the struct" when {
          "constructor" in {
            goToDefinition() {
              """
                |{
                |  let value = 1
                |  let instance = Struct { v@@alue: 2 }
                |}
                |""".stripMargin
            }
          }

          "deconstructor" when {
            "copied identifier is unique" in {
              goToDefinition() {
                """
                  |{
                  |  let value = 1
                  |  let Struct { v@@alue: copy } = instance
                  |}
                  |""".stripMargin
              }
            }

            "copied identifier is duplicate" in {
              // Special Case: It does not return "non-empty", but this test belongs here.
              goToDefinition() {
                """
                  |{
                  |  let value = 1
                  |  let Struct { value: >>va@@lue<< } = instance
                  |}
                  |""".stripMargin
              }
            }
          }
        }

        "variable is defined after the struct" when {
          "constructor" in {
            goToDefinition() {
              """
                |{
                |  let instance = Struct { v@@alue: 2 }
                |  let value = 1
                |}
                |""".stripMargin
            }
          }

          "deconstructor" when {
            "copied identifier is unique" in {
              goToDefinition() {
                """
                  |{
                  |  let Struct { v@@alue: copy } = instance
                  |  let value = 1
                  |}
                  |""".stripMargin
              }
            }

            "copied identifier is duplicate" in {
              // Special Case: It does not return "non-empty", but this test belongs here.
              goToDefinition() {
                """
                  |{
                  |  let Struct { value: >>va@@lue<< } = instance
                  |  let value = 1
                  |}
                  |""".stripMargin
              }
            }
          }
        }
      }
    }
  }

  "struct field values" when {
    "variable is defined before the struct" when {
      "constructor" in {
        goToDefinition() {
          """
            |{
            |  let >>value<< = 1
            |  let instance = Struct { value: val@@ue }
            |}
            |""".stripMargin
        }
      }

      "deconstructor" in {
        goToDefinition() {
          """
            |{
            |  let value = 1
            |  let Struct { value: >>val@@ue<< } = instance
            |}
            |""".stripMargin
        }
      }
    }

    "variable is defined after the struct" when {
      "constructor" in {
        goToDefinition() {
          """
            |{
            |  let instance = Struct { value: v@@alue }
            |  let >>value<< = 1
            |}
            |""".stripMargin
        }
      }

      "deconstructor" in {
        goToDefinition() {
          """
            |{
            |  let Struct { value: >>v@@alue<< } = instance
            |  let value = 1
            |}
            |""".stripMargin
        }
      }
    }
  }

  "go to the struct field" when {
    "syntax is well defined" when {
      "one struct field" when {
        "constructor" in {
          goToDefinition() {
            """
              |struct MyStruct { >>field<<: Value }
              |MyStruct { @@field: 123 }
              |""".stripMargin
          }
        }

        "deconstructor" when {
          "original reference" in {
            goToDefinition() {
              """
                |struct MyStruct { >>field<<: Value }
                |let MyStruct { @@field } = instance
                |""".stripMargin
            }
          }

          "field reference is copied" in {
            goToDefinition() {
              """
                |struct MyStruct { >>field<<: Value }
                |let MyStruct { @@field: 123 } = instance
                |""".stripMargin
            }
          }
        }
      }

      "two struct field" when {
        "first field is searched" when {
          "constructor" in {
            goToDefinition() {
              """
                |struct MyStruct {
                |  >>field1<<: Value,
                |  field2: Value
                |}
                |
                |MyStruct {
                |  @@field1: 123,
                |  field2: 123
                |}
                |""".stripMargin
            }
          }

          "deconstructor" in {
            goToDefinition() {
              """
                |struct MyStruct {
                |  >>field1<<: Value,
                |  field2: Value
                |}
                |
                |let MyStruct {
                |  @@field1: 123,
                |  field2: 123
                |} = instance
                |""".stripMargin
            }
          }
        }

        "second field is searched" when {
          "constructor" in {
            goToDefinition() {
              """
                |struct MyStruct {
                |  field1: Value,
                |  >>field2<<: Value
                |}
                |
                |MyStruct {
                |  field1: 123,
                |  f@@ield2: 123
                |}
                |""".stripMargin
            }
          }

          "deconstructor" in {
            goToDefinition() {
              """
                |struct MyStruct {
                |  field1: Value,
                |  >>field2<<: Value
                |}
                |
                |let MyStruct {
                |  field1: ref1,
                |  f@@ield2: ref2
                |} = instance
                |""".stripMargin
            }
          }
        }

        "nested struct's, first field is searched" when {
          "constructor" in {
            goToDefinition() {
              """
                |struct MyStruct {
                |  >>field1<<: Value,
                |  field2: Value,
                |  field3: MyStruct
                |}
                |
                |MyStruct {
                |  field1: 123,
                |  field2: 123,
                |  field3:
                |    MyStruct {
                |      f@@ield1: Value,
                |      field2: 123,
                |    }
                |}
                |""".stripMargin
            }
          }

          "deconstructor" in {
            goToDefinition() {
              """
                |struct MyStruct {
                |  >>field1<<: Value,
                |  field2: Value,
                |  field3: MyStruct
                |}
                |
                |let MyStruct {
                |  field1: 123,
                |  field2: 123,
                |  field3:
                |    MyStruct {
                |      f@@ield1: Value,
                |      field2: 123,
                |    }
                |} = instance
                |""".stripMargin
            }
          }
        }

        "nested struct's, second field is searched" when {
          "constructor" in {
            goToDefinition() {
              """
                |struct MyStruct {
                |  field1: Value,
                |  >>field2<<: Value,
                |  field3:
                |}
                |
                |MyStruct {
                |  field1: 123,
                |  field2: 123,
                |  field3:
                |    MyStruct {
                |      field1: Value,
                |      f@@ield2: 123,
                |    }
                |}
                |""".stripMargin
            }
          }

          "deconstructor" in {
            goToDefinition() {
              """
                |struct MyStruct {
                |  field1: Value,
                |  >>field2<<: Value,
                |  field3:
                |}
                |
                |let MyStruct {
                |  field1: 123,
                |  field2: 123,
                |  field3:
                |    MyStruct {
                |      field1: Value,
                |      f@@ield2: 123,
                |    }
                |} = instance
                |""".stripMargin
            }
          }
        }

        "nested struct's, third field is searched with the third field's type param not defined" when {
          "constructor" in {
            goToDefinition() {
              """
                |struct MyStruct {
                |  field1: Value,
                |  field2: Value,
                |  >>field3<<:
                |}
                |
                |MyStruct {
                |  field1: 123,
                |  field2: ,
                |  field3:
                |    MyStruct {
                |      field1: Value,
                |      field2: ,
                |      f@@ield3: 123,
                |    }
                |}
                |""".stripMargin
            }
          }

          "deconstructor" in {
            goToDefinition() {
              """
                |struct MyStruct {
                |  field1: Value,
                |  field2: Value,
                |  >>field3<<:
                |}
                |
                |let MyStruct {
                |  field1: 123,
                |  field2: ,
                |  field3:
                |    MyStruct {
                |      field1: Value,
                |      field2: ,
                |      f@@ield3: 123,
                |    }
                |} = instance
                |""".stripMargin
            }
          }
        }
      }
    }

    "field values are not provided" when {
      "one field" when {
        "constructor" in {
          goToDefinition() {
            """
              |struct MyStruct { >>field<< }
              |MyStruct { @@field }
              |""".stripMargin
          }
        }

        "deconstructor" in {
          goToDefinition() {
            """
              |struct MyStruct { >>field<< }
              |let MyStruct { @@field } = instance
              |""".stripMargin
          }
        }
      }

      "two fields" when {
        "the first field is searched" when {
          "constructor" in {
            goToDefinition() {
              """
                |struct MyStruct {
                |  >>field1<<,
                |    field2
                |}
                |
                |MyStruct {
                |  @@field1,
                |  field2
                |}
                |""".stripMargin
            }
          }

          "def constructor" in {
            goToDefinition() {
              """
                |struct MyStruct {
                |  >>field1<<,
                |    field2
                |}
                |
                |let MyStruct {
                |  @@field1,
                |  field2
                |} = instance
                |""".stripMargin
            }
          }
        }

        "the second field is searched" when {
          "constructor" in {
            goToDefinition() {
              """
                |struct MyStruct {
                |  field1,
                |  >>field2<<
                |}
                |
                |MyStruct {
                |  field1,
                |  @@field2
                |}
                |""".stripMargin
            }
          }

          "deconstructor" in {
            goToDefinition() {
              """
                |struct MyStruct {
                |  field1,
                |  >>field2<<
                |}
                |
                |let MyStruct {
                |  field1,
                |  @@field2
                |} = instance
                |""".stripMargin
            }
          }
        }
      }
    }

    "constructor's closing brace is missing" when {
      "one field" when {
        "constructor" in {
          goToDefinition() {
            """
              |struct MyStruct { >>field<< }
              |MyStruct { @@field
              |""".stripMargin
          }
        }

        "deconstructor" in {
          goToDefinition() {
            """
              |struct MyStruct { >>field<< }
              |let MyStruct { @@field = instance
              |""".stripMargin
          }
        }
      }

      "two fields" when {
        "first field is searched" when {
          "constructor" in {
            goToDefinition() {
              """
                |struct MyStruct {
                |  >>field1<<,
                |  field2
                |}
                |
                |MyStruct {
                |  @@field1,
                |  field2
                |""".stripMargin
            }
          }

          "deconstructor" in {
            goToDefinition() {
              """
                |struct MyStruct {
                |  >>field1<<,
                |  field2
                |}
                |
                |let MyStruct { @@field1, field2 = instance
                |""".stripMargin
            }
          }
        }

        "second field is searched" when {
          "constructor" in {
            goToDefinition() {
              """
                |struct MyStruct {
                |  field1,
                |  >>field2<<
                |}
                |
                |MyStruct {
                |  field1,
                |  @@field2
                |""".stripMargin
            }
          }

          "deconstructor" in {
            goToDefinition() {
              """
                |struct MyStruct {
                |  field1,
                |  >>field2<<
                |}
                |
                |let MyStruct { field1, @@field2 = instance
                |""".stripMargin
            }
          }
        }
      }
    }

    "definitions closing brace is missing" when {
      "constructor" in {
        goToDefinition() {
          """
            |struct MyStruct { >>field<<
            |{MyStruct { @@field }
            |""".stripMargin
        }
      }

      "deconstructor" in {
        goToDefinition() {
          """
            |struct MyStruct { >>field<<
            |{let MyStruct { @@field } = instance
            |""".stripMargin
        }
      }

    }

    "duplicate fields" when {
      "constructor" in {
        goToDefinition() {
          """
            |struct MyStruct {
            |  >>field<<,
            |  >>field<<,
            |  ,
            |  ,
            |}
            |
            |MyStruct { @@field }
            |""".stripMargin
        }
      }

      "deconstructor" in {
        goToDefinition() {
          """
            |struct MyStruct {
            |  >>field<<,
            |  >>field<<,
            |  ,
            |  ,
            |}
            |
            |let MyStruct { @@field } =
            |""".stripMargin
        }
      }
    }

    "duplicate structs with duplicate fields" when {
      val duplicateStructs =
        """
          |struct MyStruct {
          |  >>field<<
          |}
          |
          |// with comma
          |struct MyStruct {
          |  >>field<<,
          |}
          |
          |// no types
          |struct MyStruct {
          |  >>field<<,
          |  >>field<<,
          |}
          |
          |// type defined for the second
          |struct MyStruct {
          |  >>field<<,
          |  >>field<<: Type,
          |}
          |
          |// missing fields
          |struct MyStruct {
          |  >>field<<,
          |  ,
          |  >>field<<,
          |  ,
          |}
          |
          |// missing fields with one field type defined
          |struct MyStruct {
          |  >>field<<,
          |  ,
          |  >>field<<: Type,
          |  ,
          |  >>field<<,
          |}
          |
          |// No matching field
          |struct MyStruct {
          |  notThisGuy: Type
          |}
          |
          |""".stripMargin

      "first field is searched" when {
        "constructor" in {
          goToDefinition() {
            s"""
               |$duplicateStructs
               |
               |MyStruct { @@field }
               |""".stripMargin
          }
        }

        "deconstructor" in {
          goToDefinition() {
            s"""
               |$duplicateStructs
               |
               |let MyStruct { @@field } = instance
               |""".stripMargin
          }
        }
      }

      "second field is searched" when {
        "constructor" in {
          goToDefinition() {
            s"""
               |$duplicateStructs
               |
               |MyStruct {
               |  field,
               |  @@field
               |}
               |""".stripMargin
          }
        }

        "deconstructor" in {
          goToDefinition() {
            s"""
               |$duplicateStructs
               |
               |MyStruct {
               |  field,
               |  @@field
               |}
               |""".stripMargin
          }
        }
      }
    }
  }

}
