scalaVersion := "2.11.11"
name := "fluent_scala"
version := "0.1"

scalacOptions += "-feature"

resolvers += Resolver.sonatypeRepo("snapshots")

val akkaVersion = "2.5.6"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-cluster" % akkaVersion,
  "ch.epfl.data" %% "squid" % "0.2-SNAPSHOT",
)
