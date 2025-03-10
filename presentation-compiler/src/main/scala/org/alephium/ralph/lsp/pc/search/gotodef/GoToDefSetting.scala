// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.search.gotodef

/**
 * Settings that control go-to-definitions search behaviour.
 *
 * @param includeAbstractFuncDef If `true`, abstract functions are included in the search result.
 *                               This behaviour is similar to a go-to-implementation operation.
 *                               For example: In the following case, executing go-to-definition on either one of
 *                               the functions will return both the abstract definition and the implementation.
 *                               {{{
 *                                   Abstract Contract Parent() {
 *                                     fn >>function<<() -> ()
 *                                   }
 *
 *                                   Contract Child() extends Parent() {
 *                                     fn >>function()<< -> () { }
 *                                   }
 *                               }}}
 * @param includeInheritance     If `true`, inherited parameters are included in the search results.
 *                               If `false`, inherited parameters are excluded.
 *                               For example, in the following case, the `param` in the `Parent` contract
 *                               should be excluded from the search. To achieve this,
 *                               `enableInheritanceSearch` must be set to `false`.
 *                               {{{
 *                                  Abstract Contract Parent(param: Type)
 *
 *                                  Contract MyContract(>>param<<: Type) extends Parent(para@@m)
 *                               }}}
 */
case class GoToDefSetting(
    includeAbstractFuncDef: Boolean,
    includeInheritance: Boolean)
