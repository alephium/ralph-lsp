// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.message.SourceIndexExtra.SourceIndexExtension
import org.alephium.ralph.lsp.access.compiler.parser.soft.ast.SoftAST
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.search.cache.SearchCache
import org.alephium.ralph.lsp.pc.sourcecode.{SourceCodeState, SourceLocation}
import org.alephium.ralph.lsp.pc.workspace.TestWorkspace
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CodeProviderASTSpec extends AnyWordSpec with Matchers {

  implicit val file: FileAccess         = FileAccess.disk
  implicit val compiler: CompilerAccess = CompilerAccess.ralphc
  implicit val logger: ClientLogger     = TestClientLogger
  implicit val searchCache: SearchCache = SearchCache(maxWorkspaces = 1)

  "AST searches" should {
    Seq(
      "Contract Test(value: U256) { fn test() -> U256 { return value } }",
      "Contract Test(value: U256) {\n fn test() -> U256 {\n return value"
    ).foreach {
      code =>
        s"match positional definition searches in '$code'" in {
          val workspace = TestWorkspace.genParsed(code).sample.value
          val source    = workspace.sourceCode.head.asInstanceOf[SourceCodeState.IsParsed]
          val root      = source.astSoft.fetch().value
          val target = root
            .toNode
            .walk
            .map(_.data)
            .collect {
              case id: SoftAST.Identifier if id.code.text == "value" => id
            }
            .toList
            .last
          val settings = (SoftAST, TestCodeProvider.testGoToDefSetting)
          val positioned = CodeProvider
            .goToDef
            .search(
              target.index.toLineRange(source.code).from,
              source.fileURI,
              workspace,
              settings
            )
            .value
            .value
            .toList
          val direct = CodeProvider.goToDef.search(target, source.fileURI, workspace, settings).value.value.toList
          direct shouldBe positioned
          CodeProvider.goToDef.search(target.index.from, source.fileURI, workspace, settings).value.value.toList shouldBe direct
          direct.collect {
            case SourceLocation.NodeSoft(value: SoftAST.CodeString, _) => value.text
          } shouldBe List("value")
          CodeProvider.goToDef.search(target.code, source.fileURI, workspace, settings).value.value.toList shouldBe direct
        }
    }

    "select dependency source for direct identifier searches" in {
      val workspace = TestWorkspace.genParsed("Contract Test() {}").sample.value
      val (source, target) = workspace
        .build
        .dependencies
        .iterator
        .flatMap(_.sourceCode)
        .flatMap {
          source =>
            source.astSoft.fetch().toOption.iterator.flatMap(_.toNode.walk).collect {
              case node
                  if node.data.isInstanceOf[SoftAST.Identifier] &&
                    node.data.asInstanceOf[SoftAST.Identifier].code.text == "assert!" =>
                (source, node.data.asInstanceOf[SoftAST.Identifier])
            }
        }
        .next()
      val settings = (SoftAST, TestCodeProvider.testGoToDefSetting)
      val direct   = CodeProvider.goToDef.search(target, source.fileURI, workspace, settings).value.value.toList
      direct should not be empty
      CodeProvider.goToDef.search(target.index.toLineRange(source.code).from, source.fileURI, workspace, settings).value.value.toList shouldBe direct
    }

    "reject an identifier from a different parsed tree" in {
      val code      = "Contract Test(value: U256) { fn test() -> U256 { return value } }"
      val workspace = TestWorkspace.genParsed(code).sample.value
      val source    = workspace.sourceCode.head.asInstanceOf[SourceCodeState.IsParsed]
      val other     = TestWorkspace.genParsed(code).sample.value.sourceCode.head.asInstanceOf[SourceCodeState.IsParsed]
      val target = other
        .astSoft
        .fetch()
        .value
        .toNode
        .walk
        .map(_.data)
        .collectFirst {
          case id: SoftAST.Identifier if id.code.text == "value" => id
        }
        .value
      CodeProvider.goToDef.search(target, source.fileURI, workspace, (SoftAST, TestCodeProvider.testGoToDefSetting)).value.value.toList shouldBe empty
    }
  }

}
