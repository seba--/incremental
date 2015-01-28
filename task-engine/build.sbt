name := "task-engine"

version := "1.0"

scalaVersion := "2.11.5"

parallelExecution in Test := false

logBuffered in Test := false

libraryDependencies ++= Seq(
	"org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
    "org.scala-lang" % "scala-library" % "2.11.5",
	"org.scala-lang" % "scala-compiler" % "2.11.5"
)

testOptions in Test += Tests.Argument("-oD")

scalacOptions ++= Seq("-feature")


