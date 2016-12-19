lazy val autoargs = project
  .in(file("."))
  .settings(
    scalaVersion := "2.11.8",
    organization := "com.github.tobias-johansson",
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.2"
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.0"
    ) map (_ % "test"),
    licenses := Seq(
      "MIT" -> url("https://opensource.org/licenses/MIT")
    )
  )
