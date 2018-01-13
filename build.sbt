name := "scala-crdt"

version := "0.1"

scalaVersion := "2.12.4"

homepage := Some(url("https://github.com/greywombat/scala-crdt"))

scmInfo := Some(ScmInfo(url("https://github.com/greywombat/scala-crdt"), "git@github.com:greywombat/scala-crdt.git"))

developers := List(Developer("greywombat", "Florian Kreitmair", "florian.kreitmair@tum.de", url("https://github.com/greywombat")))

licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))

publishMavenStyle := true

libraryDependencies ++= Seq(
  "io.monix" %% "monix" % "2.3.0",
  "org.typelevel" %% "algebra" % "0.6.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.5" % "test",
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % "test")

logBuffered in (Test,test) := false

testListeners in (Test,test) := Nil
