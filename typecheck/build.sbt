

name := "typecheck"

version := "1.0"

scalaVersion := "2.11.7"

parallelExecution in Test := false

logBuffered in Test := false

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

testOptions in Test += Tests.Argument("-oD")

scalacOptions ++= Seq("-feature", "-deprecation")

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.7"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.10"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0"

fork in run := true

connectInput in run := true

javaOptions in run ++= Seq("-Xmx4096m", "-Xms2048m")

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
