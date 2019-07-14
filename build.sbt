name := "game-sandbox"
version := "0.0.1-SNAPSHOT"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

scalaVersion := "2.12.7"

scalacOptions ++= Seq(
  "-encoding", "UTF-8",   // source files are in UTF-8
  "-deprecation",         // warn about use of deprecated APIs
  "-unchecked",           // warn about unchecked type parameters
  "-feature",             // warn about misused language features
  "-language:higherKinds",// allow higher kinded types without `import scala.language.higherKinds`
  "-Xlint",               // enable handy linter warnings
  "-Xfatal-warnings",     // turn compiler warnings into errors
  "-Ypartial-unification", // allow the compiler to unify type constructors of different arities
  "-Yrangepos"
)

libraryDependencies += "org.typelevel" %% "cats-core" % "1.5.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.3.1"


libraryDependencies += "org.specs2" %% "specs2-core" % "4.6.0" % "test"


addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
