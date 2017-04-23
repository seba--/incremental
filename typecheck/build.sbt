

name := "typecheck"

version := "1.0"

scalaVersion := "2.11.8"

parallelExecution in Test := false

logBuffered in Test := false

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "com.github.javaparser" % "javaparser-core" % "3.2.1"

testOptions in Test += Tests.Argument("-oD")

scalacOptions ++= Seq("-feature", "-deprecation")

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.storm-enroute" % "scalameter_2.11" % "0.8.2"

libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.16"

libraryDependencies += "com.github.scopt" % "scopt_2.11" % "3.5.0"

libraryDependencies += "com.github.pathikrit" %% "better-files" % "2.17.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-java8-compat" % "0.8.0"

fork in Test := true

fork in run := true

connectInput in run := true

javaOptions in run ++= Seq("-Xmx4096m", "-Xms2048m")

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")


//import com.typesafe.sbt.SbtStartScript

//seq(SbtStartScript.startScriptForClassesSettings: _*)
