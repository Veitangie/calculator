lazy val root = (project in file("."))
  .settings(
    name         := "calculator",
    version      := "0.3.0",
    organization := "veitangie",
    scalaVersion := "3.2.2",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", _*) => MergeStrategy.discard
      case x                        => MergeStrategy.first
    },
    assembly / assemblyJarName := s"${name.value}-${version.value}.jar",
    libraryDependencies ++= Seq(
      "ch.obermuhlner" % "big-math"    % "2.3.2",
      "org.typelevel" %% "cats-core"   % "2.9.0",
      "org.typelevel" %% "cats-effect" % "3.4.9"
    )
  )
