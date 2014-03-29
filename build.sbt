name := "ScanRenamer"

version := "1.0"

scalaVersion := "2.10.3"

resolvers += "tpolecat" at "http://dl.bintray.com/tpolecat/maven"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6"

libraryDependencies += "org.scalaz" %% "scalaz-typelevel" % "7.0.6"

libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.0.6"

libraryDependencies += "org.tpolecat" %% "atto" % "0.1"
