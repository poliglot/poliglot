import sbt._
import sbt.Keys._

object Build extends sbt.Build {
  val buildOrganisation = "poliglot"
  val buildVersion = "0.1-SNAPSHOT"
  val buildScalaVersion = "2.11.4"
  val buildScalaOptions = Seq(
    "-unchecked", "-deprecation",
    "-encoding", "utf8")

  lazy val main = Project(id = "poliglot", base = file("."))
    .settings(
      resolvers ++= Seq(),
      libraryDependencies ++= Seq(
        "org.typelevel" %% "scodec-core" % "1.6.0"
      ),
      organization := buildOrganisation,
      version := buildVersion,
      scalaVersion := buildScalaVersion,
      scalacOptions := buildScalaOptions
    )
}