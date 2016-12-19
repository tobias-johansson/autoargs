
publishMavenStyle       := true
publishArtifact in Test := false
pomIncludeRepository    := { _ => false }
useGpg                  := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  isSnapshot.value match {
    case true  => Some("snapshots" at nexus + "content/repositories/snapshots")
    case false => Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  }
}

pomExtra := (
  <url>https://github.com/tobias-johansson/autoargs</url>
  <scm>
    <url>git@github.com:tobias-johansson/autoargs</url>
    <connection>scm:git:git@github.com:tobias-johansson/autoargs</connection>
  </scm>
  <developers>
    <developer>
      <id>tobias-johansson</id>
      <name>Tobias Johansson</name>
      <url>https://github.com/tobias-johansson</url>
    </developer>
  </developers>)
