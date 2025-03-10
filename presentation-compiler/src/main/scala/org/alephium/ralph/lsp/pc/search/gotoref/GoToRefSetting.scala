// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotoref

import org.alephium.ralph.lsp.pc.search.gotodef.GoToDefSetting

/**
 * Settings that control the behaviour of go-to-references search.
 *
 * @param includeDeclaration               If `true`, definitions and declarations are included in the search result.
 * @param includeTemplateArgumentOverrides If `true`, overridden template or contract-level arguments are included.
 *                                         For example: In the following case, the overridden `variable` in `Child` Contract
 *                                         will be included when searching for references to `variable` defined in `Parent` Contract.
 *                                         {{{
 *                                              Abstract Contract Parent(variable@@: Bool) { }
 *                                              Abstract Contract Child(>>variable<<: Bool) extends Parent(variable) { }
 *                                         }}}
 * @param includeEventFieldReferences      Creating an instance of an Event does not require named field parameter.
 *                                         Settings this to `false` will exclude event field references from the search result.
 *                                         For example, in the following case, renaming `to` should not rename `buyer`.
 *                                         But `buyer` should be included when responding to go-to-references requests.
 *                                         {{{
 *                                               event Transfer(to: Address)
 *                                               emit Transfer(buyer)
 *                                         }}}
 * @param goToDefSetting                   This setting is used for executing go-to-definition within go-to-references.
 */
case class GoToRefSetting(
    includeDeclaration: Boolean,
    includeTemplateArgumentOverrides: Boolean,
    includeEventFieldReferences: Boolean,
    goToDefSetting: GoToDefSetting)
