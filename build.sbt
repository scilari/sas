
name := "systematic-alias-sampling"

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.3"

organization := "com.scilari"

resolvers += Resolver.jcenterRepo

resolvers in ThisBuild += Resolver.mavenLocal

addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.17")

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.2" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

parallelExecution in Test := false



