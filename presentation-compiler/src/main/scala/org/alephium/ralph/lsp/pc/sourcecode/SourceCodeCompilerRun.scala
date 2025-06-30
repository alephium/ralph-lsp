// Copyright (c) Alephium
// SPDX-License-Identifier: LGPL-3.0-only

package org.alephium.ralph.lsp.pc.sourcecode

import org.alephium.ralph.lsp.access.compiler.CompilerRunGlobalState

import scala.collection.immutable.ArraySeq

/**
 * [[SourceCodeState]] produced from a compilation run ([[org.alephium.ralph.lsp.access.compiler.CompilerRunResult]]).
 *
 * @param compiledSource         The compiled source code states.
 * @param compilerRunGlobalState An instance of [[org.alephium.ralph.Ast.GlobalState]] produced by the compilation run.
 */
case class SourceCodeCompilerRun(
    compiledSource: ArraySeq[SourceCodeState.IsParsedAndCompiled],
    compilerRunGlobalState: Option[CompilerRunGlobalState])
