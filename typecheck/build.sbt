

name := "typecheck"

version := "1.0"

scalaVersion := "2.11.6"

parallelExecution in Test := false

logBuffered in Test := false

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

testOptions in Test += Tests.Argument("-oD")

scalacOptions ++= Seq("-feature")

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.6"

fork in run := true

connectInput in run := true

javaOptions in run ++= Seq("-Xmx4096m", "-Xms2048m")

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
