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
 */
case class GoToDefSetting(includeAbstractFuncDef: Boolean)
