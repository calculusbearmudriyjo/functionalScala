import sbt._

lazy val functionalScala = (project in file(".")).
  settings (
    name          := "Functional Scala",
    organization  := "net.degoes",
    version       := "0.1-SNAPSHOT",
    scalaVersion  := "2.12.6",
    initialCommands in Compile in console := """
                                               |import scalaz._
                                               |import scalaz.zio._
                                               |import scalaz.zio.console._
                                               |import net.degoes._
                                               |object replRTS extends RTS {}
                                               |import replRTS._
                                               |implicit class RunSyntax[E, A](io: IO[E, A]){ def unsafeRun: A = replRTS.unsafeRun(io) }
    """.stripMargin
  )

scalaVersion := "2.12.6"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")

scalacOptions ++= Seq(
  "-deprecation"
  , "-unchecked"
  , "-encoding", "UTF-8"
  , "-Xlint"
  , "-Xverify"
  , "-feature"
  ,"-Ypartial-unification"
  ,"-Xfatal-warnings"
  , "-language:_"
)

javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation", "-source", "1.7", "-target", "1.7")

val CatsEffectVersion = "1.0.0-RC3"
val ScalaZVersion     = "7.2.23"
val ZIOVersion        = "0.2.6"

libraryDependencies ++= Seq(
  // -- testing --
  "org.scalacheck"  %% "scalacheck"         % "1.13.4"  % "test",
  "org.scalatest"   %% "scalatest"          % "3.0.1"   % "test",
  // Cats Effect
  "org.typelevel"   %% "cats-effect"        % CatsEffectVersion,
  // Scalaz
  "org.scalaz"      %% "scalaz-core"        % ScalaZVersion,
  "org.scalaz"      %% "scalaz-zio"         % ZIOVersion,
  "org.scalaz"      %% "scalaz-zio-interop" % ZIOVersion,
  // Ammonite
  "com.lihaoyi"     %  "ammonite"           % "1.1.2"   % "test" cross CrossVersion.full,

  // Start with this one
  "org.tpolecat" %% "doobie-core"      % "0.5.3",

  // And add any of these as needed
  "io.chrisdavenport" %% "fuuid-circe"  % "0.1.2",
  "mysql" % "mysql-connector-java" % "5.1.24",
  "org.tpolecat" %% "doobie-hikari"    % "0.5.3", // HikariCP transactor.
  "org.tpolecat" %% "doobie-specs2"    % "0.5.3", // Specs2 support for typechecking statements.
  "org.tpolecat" %% "doobie-scalatest" % "0.5.3"  // ScalaTest support for typechecking statements.
)

resolvers ++= Seq(
  "Typesafe Snapshots"          at "http://repo.typesafe.com/typesafe/snapshots/",
  "Secured Central Repository"  at "https://repo1.maven.org/maven2",
  Resolver.sonatypeRepo("snapshots")
)

// Ammonite REPL
sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main().run() }""")
  Seq(file)
}.taskValue
