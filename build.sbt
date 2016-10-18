name := "systematic-alias-sampling"

version := "1.0"

scalaVersion := "2.11.8"

resolvers += Resolver.jcenterRepo

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

parallelExecution in Test := false


