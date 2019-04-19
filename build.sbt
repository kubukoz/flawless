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
  compilerPlugin("org.spire-math" %% "kind-projector"     % "0.9.10"),
  compilerPlugin("com.olegpy"     %% "better-monadic-for" % "0.3.0")
)

val commonSettings = Seq(
  scalaVersion := "2.12.8",
  scalacOptions ++= Options.all,
  name := "flawless",
  libraryDependencies ++= List(
    "org.typelevel"     %% "cats-effect"  % "1.2.0",
    "com.github.gvolpe" %% "console4cats" % "0.6.0"
  ) ++ compilerPlugins
)

val core = project.settings(commonSettings).settings(name += "-core")

val flawless =
  project
    .in(file("."))
    .settings(commonSettings)
    .settings(skip in publish := true)
    .dependsOn(core)
    .aggregate(core)
