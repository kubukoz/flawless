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
    )
  )
)

val compilerPlugins = List(
  compilerPlugin("org.scalamacros" % "paradise" % "2.1.1").cross(CrossVersion.full),
  compilerPlugin("org.typelevel" % "kind-projector" % "0.11.0").cross(CrossVersion.full),
  compilerPlugin("com.kubukoz" % "better-tostring" % "0.2.2").cross(CrossVersion.full),
  compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
)

val commonSettings = Seq(
  scalaVersion := "2.12.11",
  name := "flawless",
  scalacOptions -= "-Xfatal-warnings",
  libraryDependencies ++= List(
    "co.fs2" %% "fs2-core" % "2.3.0",
    "com.olegpy" %% "meow-mtl-effects" % "0.4.0",
    "com.olegpy" %% "meow-mtl-core" % "0.4.0",
    "org.typelevel" %% "cats-tagless-macros" % "0.11",
    "com.github.julien-truffaut" %% "monocle" % "2.0.3",
    "org.typelevel" %% "cats-effect" % "2.1.3",
    "dev.profunktor" %% "console4cats" % "0.8.1",
    "com.softwaremill.diffx" %% "diffx-core" % "0.3.28"
  ) ++ compilerPlugins
)

val noPublish = Seq(skip in publish := true)

val core = project.settings(commonSettings).settings(name += "-core")

val tests =
  project
    .settings(
      commonSettings,
      libraryDependencies ++= List(
        "com.softwaremill.diffx" %% "diffx-cats" % "0.3.28"
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
        "org.tpolecat" %% "doobie-hikari" % "0.9.0",
        "org.postgresql" % "postgresql" % "42.2.12"
      )
    )
    .settings(name += "-tests", skip in publish := true)
    .settings(noPublish)
    .dependsOn(core)

val flawless =
  project.in(file(".")).settings(commonSettings).settings(noPublish).aggregate(core, examples, tests)
