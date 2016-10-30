
name := "systematic-alias-sampling"

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.1"

organization := "com.scilari"

resolvers += Resolver.jcenterRepo

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.2" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

parallelExecution in Test := false



