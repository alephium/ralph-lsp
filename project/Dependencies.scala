import sbt._

object Version {
  val scala213 = "2.13.13"
  val web3 = "0.36.1"
}
object Dependencies {

  /** TEST */
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.18" % Test
  lazy val scalaCheck = "org.scalatestplus" %% "scalacheck-1-17" % "3.2.17.0" % Test
  lazy val scalaMock = "org.scalamock" %% "scalamock" % "5.2.0" % Test

  /** Core */
  lazy val ralphc = "org.alephium" %% "alephium-ralphc" % "2.10.3"
  lazy val lsp4j = "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.22.0"

  /** Logging */
  lazy val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
  lazy val logback = "ch.qos.logback" % "logback-classic" % "1.5.3"
}
