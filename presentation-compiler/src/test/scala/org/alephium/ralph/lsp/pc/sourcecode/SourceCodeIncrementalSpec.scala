// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.utils.LazyVal
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.net.URI
import scala.collection.immutable.ArraySeq

class SourceCodeIncrementalSpec extends AnyWordSpec with Matchers with MockFactory {

  private val uri      = URI.create("file:///workspace/contracts/test.ral")
  private val original = "Contract A() {}\nContract B() {}"

  "client source updates" should {
    "route an evaluated prior soft parse through incremental parsing lazily" in {
      implicit val file: FileAccess         = null
      implicit val compiler: CompilerAccess = mock[CompilerAccess]
      val ast                               = CompilerAccess.ralphc.parseSoft(original).toOption.get
      val previous                          = SourceCodeState.ErrorParser(uri, original, Seq.empty, LazyVal(Right(ast)))
      previous.astSoft.fetch() shouldBe Right(ast)
      val updated    = original.replace("B", "Changed")
      val expected   = CompilerAccess.ralphc.parseSoft(updated)
      val uncompiled = SourceCode.putOrRemove(uri, Some(updated), ArraySeq(previous)).head.asInstanceOf[SourceCodeState.UnCompiled]
      uncompiled.previousSoft shouldBe Some(SourceCodeState.SoftParseSnapshot(original, ast))
      (compiler.parseStrict _).expects(uri, updated).returning(CompilerAccess.ralphc.parseStrict(uri, updated))
      (compiler.parseSoftIncremental _).expects(updated, original, ast).returning(expected).once()
      val parsed = SourceCode.parse(uncompiled).asInstanceOf[SourceCodeState.IsParsed]
      parsed.astSoft.isEmpty shouldBe true
      parsed.astSoft.fetch() shouldBe expected
      parsed.astSoft.fetch() shouldBe expected
    }
    "avoid forcing or retaining an unevaluated prior parse" in {
      implicit val file: FileAccess = null
      val previous                  = SourceCodeState.ErrorParser(uri, original, Seq.empty, LazyVal(throw new AssertionError("old parse forced")))
      val updated                   = SourceCode.putOrRemove(uri, Some(original + "\n"), ArraySeq(previous)).head.asInstanceOf[SourceCodeState.UnCompiled]
      updated.previousSoft shouldBe None
      previous.astSoft.isEmpty shouldBe true
    }
  }

}
