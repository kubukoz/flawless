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
  compilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
  compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
)

val commonSettings = Seq(
  scalaVersion := "2.12.10",
  scalacOptions ++= Options.all,
  name := "flawless",
  libraryDependencies ++= List(
    "org.typelevel" %% "cats-tagless-macros" % "0.10",
    "org.typelevel" %% "cats-mtl-core" % "0.7.0",
    "com.github.julien-truffaut" %% "monocle-macro" % "2.0.0",
    "org.typelevel" %% "cats-effect" % "2.0.0",
    "dev.profunktor" %% "console4cats" % "0.8.0",
    "com.lihaoyi" %% "sourcecode" % "0.1.7",
    "com.lihaoyi" %% "pprint" % "0.5.5", //todo temporary
    "com.softwaremill.diffx" %% "diffx-core" % "0.3.7"
  ) ++ compilerPlugins
)

val noPublish = Seq(skip in publish := true)

val core = project.settings(commonSettings).settings(name += "-core")

val tests =
  project
    .settings(commonSettings)
    .settings(name += "-tests")
    .settings(noPublish)
    .dependsOn(core)

val examples =
  project
    .settings(
      commonSettings,
      libraryDependencies ++= List(
        "org.tpolecat" %% "doobie-hikari" % "0.8.4",
        "org.postgresql" % "postgresql" % "42.2.8"
      )
    )
    .settings(name += "-tests", skip in publish := true)
    .settings(noPublish)
    .dependsOn(core)

val flawless =
  project
    .in(file("."))
    .settings(commonSettings)
    .settings(noPublish)
    .aggregate(core, examples, tests)
