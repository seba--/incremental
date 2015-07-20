
name := "typecheck-object-algebra"

version := "1.0"

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := "2.11.2"

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-Yvirtualize"
)

libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % "2.11.2"

libraryDependencies += "org.scala-lang.virtualized" % "scala-library" % "2.11.2"

libraryDependencies += "org.scala-lang.virtualized" % "scala-reflect" % "2.11.2"

libraryDependencies += "EPFL" %% "lms" % "0.3-SNAPSHOT"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.2"

libraryDependencies += "org.scala-lang.virtualized" % "scala-actors" % "2.11.2" % "test"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.6"


testOptions in Test += Tests.Argument("-oD")

fork in run := true

connectInput in run := true

javaOptions in run ++= Seq("-Xmx4096m", "-Xms2048m")

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
