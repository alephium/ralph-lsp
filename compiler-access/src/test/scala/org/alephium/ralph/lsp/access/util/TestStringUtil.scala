package org.alephium.ralph.lsp.access.util

object TestStringUtil {
 def codeLines(code: String): Array[String] =
    code.split("\r\n|\r|\n")
}
