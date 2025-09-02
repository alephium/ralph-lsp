// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

import org.alephium.ralph.lsp.pc.search.TestCodeProvider._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

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
        "variable is defined before the struct" in {
          goToDefinition() {
            """
              |{
              |  let value = 1
              |  let instance = Struct { v@@alue: 2 }
              |}
              |""".stripMargin
          }
        }

        "variable is defined after the struct" in {
          goToDefinition() {
            """
              |{
              |  let instance = Struct { v@@alue: 2 }
              |  let value = 1
              |}
              |""".stripMargin
          }
        }
      }
    }
  }

  "struct field values" when {
    "variable is defined before the struct" in {
      goToDefinition() {
        """
          |{
          |  let >>value<< = 1
          |  let instance = Struct { value: val@@ue }
          |}
          |""".stripMargin
      }
    }

    "variable is defined after the struct" in {
      goToDefinition() {
        """
          |{
          |  let instance = Struct { value: v@@alue }
          |  let >>value<< = 1
          |}
          |""".stripMargin
      }
    }
  }

  "go to the struct field" when {
    "syntax is well defined" when {
      "one struct field" in {
        goToDefinition() {
          """
            |struct MyStruct { >>field<<: Value }
            |MyStruct { @@field: 123 }
            |""".stripMargin
        }
      }

      "two struct field" when {
        "first field is searched" in {
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

        "second field is searched" in {
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

        "nested struct's, first field is searched" in {
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

        "nested struct's, second field is searched" in {
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

        "nested struct's, third field is searched with the third field's type param not defined" in {
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
      }
    }

    "field values are not provided" when {
      "one field" in {
        goToDefinition() {
          """
            |struct MyStruct { >>field<< }
            |MyStruct { @@field }
            |""".stripMargin
        }
      }

      "two fields" when {
        "the first field is searched" in {
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

        "the second field is searched" in {
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
      }
    }

    "constructor's closing brace is missing" when {
      "one field" in {
        goToDefinition() {
          """
            |struct MyStruct { >>field<< }
            |MyStruct { @@field
            |""".stripMargin
        }
      }

      "two fields" when {
        "first field is searched" in {
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

        "second field is searched" in {
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
      }
    }

    "definitions closing brace is missing" in {
      goToDefinition() {
        """
          |struct MyStruct { >>field<<
          |{MyStruct { @@field }
          |""".stripMargin
      }
    }

    "duplicate fields" in {
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

      "first field is searched" in {
        goToDefinition() {
          s"""
            |$duplicateStructs
            |
            |MyStruct { @@field }
            |""".stripMargin
        }
      }

      "second field is searched" in {
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
