import sbt._

object Version {

  val scala213 = "2.13.13"
  val web3     = "0.38.0"

}

object Dependencies {

  /** TEST */
  lazy val scalaTest  = "org.scalatest"     %% "scalatest"       % "3.2.18"   % Test
  lazy val scalaCheck = "org.scalatestplus" %% "scalacheck-1-17" % "3.2.18.0" % Test
  lazy val scalaMock  = "org.scalamock"     %% "scalamock"       % "6.0.0"    % Test

  /** Core */
  lazy val ralphc = "org.alephium"     %% "alephium-ralphc"   % "2.12.0+0-9f1f4f5b+20240416-1132-SNAPSHOT"
  lazy val lsp4j  = "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.22.0"

  /** Logging */
  lazy val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging"   % "3.9.5"
  lazy val logback      = "ch.qos.logback"              % "logback-classic" % "1.5.3"

}
