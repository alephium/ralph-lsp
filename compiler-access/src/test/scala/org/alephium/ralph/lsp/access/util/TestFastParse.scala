// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.access.util

import org.alephium.ralph.error.CompilerError
import org.scalatest.TryValues._
import org.scalatest.matchers.should.Matchers._
import org.scalatest.Assertion

import scala.util.Try

object TestFastParse {

  def assertIsFastParseError[A](f: => A): Assertion =
    Try(f)
      .failure
      .exception
      .getCause shouldBe a[CompilerError.FastParseError]

}
