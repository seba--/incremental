

name := "typecheck"

version := "1.0"

scalaVersion := "2.11.5"

parallelExecution in Test := false

logBuffered in Test := false

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

testOptions in Test += Tests.Argument("-oD")

scalacOptions ++= Seq("-feature")

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.6"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
