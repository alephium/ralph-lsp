import sbt._

object Version {
  val scala213 = "2.13.11"
}

object Dependencies {

  /** TEST */
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.16" % Test
  lazy val scalaCheck = "org.scalatestplus" %% "scalacheck-1-17" % "3.2.16.0" % Test
  lazy val scalaMock = "org.scalamock" %% "scalamock" % "5.2.0" % Test

  /** Core */
  lazy val ralphc = "org.alephium" %% "alephium-ralphc" % "2.0.0+206-cc55f429+20230825-0041-SNAPSHOT"
  lazy val lsp4j = "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.21.1"

  /** Logging */
  lazy val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
  lazy val logback = "ch.qos.logback" % "logback-classic" % "1.4.7"

}
