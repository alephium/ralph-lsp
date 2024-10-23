// Copyright 2024 The Alephium Authors
// This file is part of the alephium project.
//
// The library is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// The library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with the library. If not, see http://www.gnu.org/licenses/.

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.pc.search.gotodef.GoToDefSetting

/**
 * Settings that control go-to-references search behaviour.
 *
 * @param includeDeclaration               If true, includes definitions/declarations to the search result.
 * @param includeTemplateArgumentOverrides If true, includes overridden template/Contract level arguments.
 *                                         For example: In the following case the overridden `variable` in `Child` Contract
 *                                         will be included when searching for references on `variable` defined in `Parent` Contract.
 *                                         {{{
 *                                              Abstract Contract Parent(variable@@: Bool) { }
 *                                              Abstract Contract Child(>>variable<<: Bool) extends Parent(variable) { }
 *                                         }}}
 * @param includeEventFieldReferences      Creating an instance of an Event does not require named field parameter.
 *                                         Settings this to false will exclude event field references from the search result.
 *                                         For example, in the following case, renaming `to` should not rename `buyer`.
 *                                         But `buyer` should be included when responding to go-to-references requests.
 *                                         {{{
 *                                               event Transfer(to: Address)
 *                                               emit Transfer(buyer)
 *                                         }}}
 * @param goToDefSetting                   This setting will be used for executing Go-to-definitions which is sometimes
 *                                         accessed by go-to-references to simplify reference search.
 */
case class GoToRefSetting(
    includeDeclaration: Boolean,
    includeTemplateArgumentOverrides: Boolean,
    includeEventFieldReferences: Boolean,
    goToDefSetting: GoToDefSetting)
