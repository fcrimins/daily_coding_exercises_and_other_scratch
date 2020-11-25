// http://xerial.org/blog/2014/03/24/sbt/
// https://www.jetbrains.com/help/idea/2016.2/getting-started-with-sbt.html

name := "daily_coding_exercises_and_other_scratch"

version := "1.0"

scalaVersion := "2.11.8"

// library dependencies: (groupId) % (artifactId) % (version)
libraryDependencies ++= Seq(
  //"org.scala-lang" % "scala-library" % scalaVersion.value % "compile",
  "org.apache.logging.log4j" % "log4j-api" % "2.7",
  "org.apache.logging.log4j" % "log4j-core" % "2.7",
  "org.apache.spark" % "spark-core_2.11" % "2.0.0", // would be nice to "Getting the right Scala version with %%"
  "org.apache.spark" % "spark-mllib_2.11" % "2.0.0", // per here http://www.scala-sbt.org/1.0/docs/Library-Dependencies.html
  "org.apache.spark" % "spark-streaming_2.11" % "2.0.0"
  //"org.apache.commons" % "commons-math3" % "3.1.1",
  //"org.fluentd" % "fluent-logger" % "0.2.10",
  //"org.mockito" % "mockito-core" % "1.9.5" % "test"  // Test-only dependency
)
