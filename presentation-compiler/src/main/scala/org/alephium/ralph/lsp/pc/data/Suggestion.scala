package org.alephium.ralph.lsp.pc.data

sealed trait Suggestion extends Product {
  def label: String

  def insert: String

  def detail: String

  def documentation: String
}

object Suggestion {
  case class Function(label: String,
                      insert: String,
                      detail: String,
                      documentation: String) extends Suggestion

  case class Field(label: String,
                   insert: String,
                   detail: String,
                   documentation: String) extends Suggestion
}
