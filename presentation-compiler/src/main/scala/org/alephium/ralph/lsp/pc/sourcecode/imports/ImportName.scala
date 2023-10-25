package org.alephium.ralph.lsp.pc.sourcecode.imports

case class ImportName private(value: String) extends AnyVal {
  override def toString:String = value
}
object ImportName {
  def cleaned(value:String): ImportName = {
    //Remove start and end quote if exist
    val tmp = value.replaceAll("^\"|\"$", "");
    if(tmp.endsWith(".ral")) {
      ImportName(tmp.dropRight(4))
    } else {
      ImportName(tmp)
    }
  }
}
