scalaVersion := "2.12.8"

name := "pdg-sdg"
organization := "br.ufpe.cin"

version := "0.4.41"

githubOwner := "spgroup"
githubRepository := "pdg-sdg"
githubTokenSource := TokenSource.GitConfig("github.token")

parallelExecution in Test := false

resolvers += "Maven Central" at "https://repo1.maven.org/maven2/"
resolvers += "SPG Maven Repository" at "https://maven.pkg.github.com/spgroup/soot/"
resolvers += "SVFA releases" at "https://maven.pkg.github.com/spgroup/svfa-scala/"
resolvers += "Local maven repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

resolvers += Classpaths.typesafeReleases

libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0"
libraryDependencies += "org.soot-oss" % "soot" % "4.5.1"
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
