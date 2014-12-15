

name := "typecheck"

version := "1.0"

scalaVersion := "2.11.4"

parallelExecution in Test := false

logBuffered in Test := false

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

testOptions in Test += Tests.Argument("-oD")

scalacOptions ++= Seq("-feature")

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.7-SNAPSHOT"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
