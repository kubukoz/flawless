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
  compilerPlugin("org.typelevel" % "kind-projector" % "0.11.3").cross(CrossVersion.full),
  compilerPlugin("com.kubukoz" % "better-tostring" % "0.2.7").cross(CrossVersion.full),
  compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
)

val commonSettings = Seq(
  scalaVersion := "2.12.11",
  name := "flawless",
  scalacOptions -= "-Xfatal-warnings",
  libraryDependencies ++= List(
    "co.fs2" %% "fs2-core" % "3.0.0-M9",
    "org.typelevel" %% "cats-tagless-macros" % "0.12",
    "org.typelevel" %% "cats-mtl" % "1.1.3",
    "com.github.julien-truffaut" %% "monocle" % "2.0.3",
    "org.typelevel" %% "cats-effect-std" % "3.0.0-RC2",
    "org.typelevel" %% "cats-effect-kernel" % "3.0.0-RC2",
    "com.softwaremill.diffx" %% "diffx-core" % "0.3.30"
  ) ++ compilerPlugins
)

val noPublish = Seq(skip in publish := true)

val core = project.settings(commonSettings).settings(name += "-core")

val tests =
  project
    .settings(
      commonSettings,
      libraryDependencies ++= List(
        "org.typelevel" %% "cats-effect" % "3.0.0-RC2",
        "com.softwaremill.diffx" %% "diffx-cats" % "0.4.2"
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
        "org.typelevel" %% "cats-effect" % "3.0.0-RC2",
        //this won't work for now
        "org.tpolecat" %% "doobie-hikari" % "0.9.4",
        "org.postgresql" % "postgresql" % "42.2.19"
      )
    )
    .settings(name += "-tests", skip in publish := true)
    .settings(noPublish)
    .dependsOn(core)

val flawless =
  project.in(file(".")).settings(commonSettings).settings(noPublish).aggregate(core, examples, tests)
