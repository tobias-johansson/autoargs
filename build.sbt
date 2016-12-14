
lazy val app = project
  .in(file("."))
  .settings(
    name         := "args",
    scalaVersion := "2.11.8",
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.2"
    )
  )
