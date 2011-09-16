name := "lisp"

scalaVersion := "2.9.1"

resolvers += "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.9.1" % "1.6.1"
)
