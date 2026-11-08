// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search

import org.alephium.ralph.lsp.pc.TestPCStates
import org.alephium.ralph.lsp.pc.client.TestClientLogger
import org.alephium.ralph.lsp.pc.search.cache.SearchCache
import org.alephium.ralph.lsp.utils.IsCancelled
import org.alephium.ralph.lsp.utils.log.ClientLogger
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.net.URI
import java.nio.file.Paths
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

class VirtualDocumentSpec extends AnyWordSpec with Matchers {

  implicit val searchCache: SearchCache = SearchCache(maxWorkspaces = 1)
  implicit val logger: ClientLogger     = TestClientLogger
  implicit val ec: ExecutionContext     = ExecutionContext.Implicits.global

  "virtual document requests" should {
    "return no results instead of loading a Java filesystem provider" in {
      val states = TestPCStates.genPCStates(Iterable(Paths.get("/workspace").toUri))
      val uri    = new URI("vscode-local-history:/workspace/contract.ral?history=1")
      val requests = Seq(
        MultiCodeProvider
          .hover
          .search(uri,
                  0,
                  0,
                  IsCancelled(
                    () => false
                  ),
                  states,
                  ()
          ),
        MultiCodeProvider
          .goToDef
          .search(uri,
                  0,
                  0,
                  IsCancelled(
                    () => false
                  ),
                  states,
                  ()
          ),
        MultiCodeProvider
          .completion
          .search(uri,
                  0,
                  0,
                  IsCancelled(
                    () => false
                  ),
                  states,
                  ()
          ),
        MultiCodeProvider
          .rename
          .search(uri,
                  0,
                  0,
                  IsCancelled(
                    () => false
                  ),
                  states,
                  ()
          )
      )
      requests.foreach {
        request =>
          Await.result(request, 10.seconds).map(_.isEmpty) shouldBe Right(true)
      }
    }
  }

}
