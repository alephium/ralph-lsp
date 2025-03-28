// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.workspace

import org.alephium.ralph.lsp.access.compiler.CompilerAccess
import org.alephium.ralph.lsp.access.compiler.ast.Tree
import org.alephium.ralph.lsp.access.file.FileAccess
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.alephium.ralph.lsp.pc.sourcecode.{SourceLocation, TestSourceCode}
import org.alephium.ralph.lsp.pc.workspace.build.TestBuild
import org.scalatest.OptionValues._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.ArraySeq

class WorkspaceSearcherCollectInheritedParentsSpec extends AnyWordSpec with Matchers {

  implicit val file: FileAccess         = FileAccess.disk
  implicit val compiler: CompilerAccess = CompilerAccess.ralphc
  implicit val logger: ClientLogger     = TestClientLogger

  "contain all source-code in scope" in {
    // Generate a build file
    val build =
      TestBuild.genCompiledOK().sample.value

    // Generate a source file which contains the contract Child1
    val sourceFile1 =
      TestSourceCode
        .genParsedOK(
          code = """
            |import "std/nft_interface"
            |import "std/nft_collection_interface"
            |
            |// Note: INFT interface is implemented
            |Abstract Contract Parent1() extends Parent2() implements INFT { }
            |
            |Contract Child() extends Parent1() {
            |  fn function() -> () {}
            |}
            |""".stripMargin,
          fileURI = build.contractURI.resolve("file1.ral")
        )
        .sample
        .value

    TestSourceCode persist sourceFile1

    // Generate a source file which contains some of the parent inheritances
    val sourceFile2 =
      TestSourceCode
        .genParsedOK(
          code = """
            |import "std/nft_collection_with_royalty_interface"
            |
            |// Note: INFTCollectionWithRoyalty is implemented
            |Abstract Contract Parent2() implements INFTCollectionWithRoyalty { }
            |""".stripMargin,
          fileURI = build.contractURI.resolve("file2.ral")
        )
        .sample
        .value

    TestSourceCode persist sourceFile2

    // create a workspace for the two source files above
    val workspace = {
      val unCompiled =
        WorkspaceState.UnCompiled(
          build = build,
          sourceCode = ArraySeq(sourceFile1, sourceFile2)
        )

      // it is successfully parsed
      val workspace =
        Workspace
          .parse(unCompiled)
          .asInstanceOf[WorkspaceState.Parsed]

      // it contains the 2 source files
      workspace.sourceCode should have size 2

      workspace
    }

    // collect all trees from file1
    val file1Trees =
      sourceFile1.astStrict.statements.collect {
        case source: Tree.Source =>
          SourceLocation.CodeStrict(source, sourceFile1)
      }

    // We need to test to find in-scope inheritance for the Child contract.
    val childTree =
      file1Trees.find(_.tree.ast.name == "Child").value

    // collect all trees from file2
    val file2Trees =
      sourceFile2.astStrict.statements.collect {
        case source: Tree.Source =>
          SourceLocation.CodeStrict(source, sourceFile2)
      }

    // std interfaces with the following import identifiers should get included.
    val expectedImports =
      Array(
        "\"std/nft_interface\"",
        "\"std/nft_collection_interface\"",
        "\"std/nft_collection_with_royalty_interface\""
      )

    // find the source files with the above imports
    val dependencyTrees =
      build
        .dependencies
        .flatMap(_.sourceCode)
        .collect {
          case source if expectedImports.contains(source.importIdentifier.value.string.value) =>
            source.parsed.astStrict.statements.collect {
              case tree: Tree.Source =>
                SourceLocation.CodeStrict(
                  tree = tree,
                  parsed = source.parsed
                )
            }
        }
        .flatten

    dependencyTrees should have size 3

    // all expected source files in-scope
    val expected =
      file1Trees ++ file2Trees ++ dependencyTrees

    // execute the function
    val actual =
      WorkspaceSearcher.collectInheritedParents(
        sourceCode = SourceLocation.CodeStrict(
          tree = childTree.tree,
          parsed = sourceFile1
        ),
        workspace = workspace
      )

    // it should contain all expected source-trees.
    actual.parentTrees should contain theSameElementsAs expected

    TestBuild deleteDirectory build
  }

}
