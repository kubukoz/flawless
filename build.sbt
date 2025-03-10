inThisBuild(
  List(
    organization := "com.kubukoz",
    homepage := Some(url("https://github.com/kubukoz/flawless")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "kubukoz",
        "Jakub Kozłowski",
        "kubukoz@gmail.com",
        url("https://kubukoz.com")
      )
    ),
    fork := true
  )
)

val compilerPlugins = List(
  compilerPlugin("org.typelevel" % "kind-projector" % "0.13.3").cross(CrossVersion.full),
  compilerPlugin("org.polyvariant" % "better-tostring" % "0.3.17").cross(CrossVersion.full),
  compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
)

val commonSettings = Seq(
  scalaVersion := "2.13.16",
  name := "flawless",
  scalacOptions -= "-Xfatal-warnings",
  scalacOptions += "-Ymacro-annotations",
  libraryDependencies ++= List(
    "co.fs2" %% "fs2-core" % "3.0.2",
    "org.typelevel" %% "cats-tagless-macros" % "0.14.0",
    "org.typelevel" %% "cats-mtl" % "1.2.1",
    "com.github.julien-truffaut" %% "monocle" % "2.0.3",
    "org.typelevel" %% "cats-effect-std" % "3.2.9",
    "org.typelevel" %% "cats-effect-kernel" % "3.2.9",
    "com.softwaremill.diffx" %% "diffx-core" % "0.3.30"
  ) ++ compilerPlugins
)

val noPublish = Seq((publish / skip) := true)

val core = project.settings(commonSettings).settings(name += "-core")

val tests =
  project
    .settings(
      commonSettings,
      libraryDependencies ++= List(
        "org.typelevel" %% "cats-effect" % "3.2.9",
        "com.softwaremill.diffx" %% "diffx-cats" % "0.3.30"
      )
    )
    .settings(name += "-tests")
    .settings(noPublish)
    .dependsOn(core)

val examples =
  project
    .settings(
      commonSettings,
      libraryDependencies ++= List(
        "org.typelevel" %% "cats-effect" % "3.2.9",
        //this won't work for now
        "org.tpolecat" %% "doobie-hikari" % "1.0.0-RC2",
        "org.postgresql" % "postgresql" % "42.2.20"
      )
    )
    .settings(name += "-tests", (publish / skip) := true)
    .settings(noPublish)
    .dependsOn(core)

val flawless =
  project.in(file(".")).settings(commonSettings).settings(noPublish).aggregate(core, examples, tests)
