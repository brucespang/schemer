name := "lisp"

scalaVersion := "2.9.1"

resolvers += "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"

resolvers += "Sonatype" at "https://oss.sonatype.org/content/repositories/"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.9.1" % "1.7.1",
  "com.codecommit" % "gll-combinators_2.9.1" % "2.0"
)
