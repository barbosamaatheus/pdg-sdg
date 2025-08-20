scalaVersion := "2.12.8"

name := "pdg-sdg"
organization := "br.ufpe.cin"

version := "0.7.0"

githubOwner := "spgroup"
githubRepository := "pdg-sdg"
githubTokenSource := TokenSource.GitConfig("github.token")

parallelExecution in Test := false

resolvers += "soot snapshots" at "https://soot-build.cs.uni-paderborn.de/nexus/repository/soot-snapshot/"

resolvers += "soot releases" at "https://soot-build.cs.uni-paderborn.de/nexus/repository/soot-release/"

resolvers += "Local maven repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

resolvers += Classpaths.typesafeReleases

resolvers += "SVFA releases" at "https://maven.pkg.github.com/galilasmb/svfa-scala/"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0"

libraryDependencies += "ca.mcgill.sable" % "soot" % "3.3.0"
libraryDependencies += "com.google.guava" % "guava" % "27.1-jre"
libraryDependencies += "org.scala-graph" %% "graph-core" % "1.13.0"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

libraryDependencies += "javax.servlet" % "javax.servlet-api" % "3.0.1" % "provided"

libraryDependencies += "javax.servlet" % "servlet-api" % "2.5" % "provided"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "br.unb.cic" % "svfa-scala_2.12" % "0.5.13"

parallelExecution in Test := false
