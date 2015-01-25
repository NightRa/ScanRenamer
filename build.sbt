name := "ScanRenamer"

version := "1.0"

scalaVersion := "2.10.4"

resolvers += "tpolecat" at "http://dl.bintray.com/tpolecat/maven"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0"

libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.1.0"

libraryDependencies += "org.tpolecat" %% "atto-core" % "0.4.1"
