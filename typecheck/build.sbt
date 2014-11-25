

name := "typecheck"

version := "1.0"

scalaVersion := "2.11.4"

parallelExecution in Test := false

logBuffered in Test := false

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

testOptions in Test += Tests.Argument("-oD")

scalacOptions ++= Seq("-feature")
