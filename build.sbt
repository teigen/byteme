name := "byteme"

organization := "com.jteigen"

version := "0.1-SNAPSHOT"

scalaVersion := "2.9.1"

crossScalaVersions := Seq("2.9.0", "2.9.1", "2.9.2")

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.9" % "test"

libraryDependencies += "org.mongodb" % "mongo-java-driver" % "2.7.3" % "test"
