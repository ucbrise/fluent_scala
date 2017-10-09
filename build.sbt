scalaVersion := "2.12.3"
name := "fluent_scala"
version := "0.1"

val akkaVersion = "2.5.6"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-cluster" % akkaVersion,
)
