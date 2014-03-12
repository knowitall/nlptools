organization := "edu.washington.cs.knowitall.nlptools"

name := "nlptools-server"

description := "A simple REST server that connects multiple NLP tool servers."

version := "1.0.0-SNAPSHOT"

crossScalaVersions := Seq("2.10.3")

scalaVersion <<= crossScalaVersions { (vs: Seq[String]) => vs.head }

libraryDependencies ++= Seq(
    "com.github.scopt" %% "scopt" % "3.1.0",
    "net.databinder" %% "unfiltered-filter" % "0.7.0",
    "net.databinder" %% "unfiltered-jetty" % "0.7.0",
    "net.databinder.dispatch" %% "dispatch-core" % "0.11.0",
    "org.slf4j" % "slf4j-api" % "1.7.5",
    "ch.qos.logback" % "logback-core" % "1.0.13",
    "ch.qos.logback" % "logback-classic" % "1.0.13")

licenses := Seq("BSD 3-clause License" -> url("http://www.opensource.org/licenses/bsd-3-clause"))
