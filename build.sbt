// http://xerial.org/blog/2014/03/24/sbt/
// https://www.jetbrains.com/help/idea/2016.2/getting-started-with-sbt.html

name := "projecteuler_scala"

version := "1.0"

scalaVersion := "2.12.0"

// library dependencies: (groupId) % (artifactId) % (version)
libraryDependencies ++= Seq(
  "org.apache.logging.log4j" % "log4j-api" % "2.7",
  "org.apache.logging.log4j" % "log4j-core" % "2.7"
  //"org.apache.commons" % "commons-math3" % "3.1.1",
  //"org.fluentd" % "fluent-logger" % "0.2.10",
  //"org.mockito" % "mockito-core" % "1.9.5" % "test"  // Test-only dependency
)
