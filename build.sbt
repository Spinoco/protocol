import xerial.sbt.Sonatype.sonatypeCentralHost

val ReleaseTag = """^release/([\d\.]+a?)$""".r

lazy val contributors = Seq(
 "pchlupacek" -> "Pavel Chlupáček"
  , "mrauilm" -> "Milan Raulim"
  , "eikek" -> "Eike Kettner"
  , "d6y" -> "Richard Dallaway"
  , "AdamChlupacek" -> "Adam Chlupáček"
)


lazy val commonSettings = Seq(
   organization := "com.spinoco",
   scalaVersion := "2.13.16",
   crossScalaVersions := Seq("2.12.20", "2.13.16"),
   scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
    "-language:postfixOps",
    "-Xfatal-warnings",
    "-Ywarn-value-discard"
   ),
   Compile / console / scalacOptions ~= {_.filterNot("-Ywarn-unused-import" == _).filterNot("-Ywarn-value-discard" == _)},
   Test / console / scalacOptions := (Compile / console / scalacOptions).value,
   libraryDependencies ++= Seq(
     "org.scodec" %% "scodec-bits" % "1.2.4"
     , "org.scodec" %% "scodec-core" % "1.11.11"
     , "org.scalatest" %% "scalatest" % "3.0.8" % "test"
     , "org.scalacheck" %% "scalacheck" % "1.14.3" % "test"
   ),
   scmInfo := Some(ScmInfo(url("https://github.com/Spinoco/protocol"), "git@github.com:Spinoco/protocol.git")),
   homepage := None,
   licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
   initialCommands := s"""
  """
) ++ testSettings ++ scaladocSettings ++ publishingSettings ++ releaseSettings

lazy val testSettings = Seq(
  Test / parallelExecution := false,
  Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
  Test / publishArtifact := true,
  Test / fork := true
)

lazy val scaladocSettings = Seq(
   Compile / doc / scalacOptions ++= Seq(
    "-doc-source-url", scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath", (LocalRootProject / baseDirectory).value.getAbsolutePath,
    "-implicits",
    "-implicits-show-all"
  ),
   Compile / doc / scalacOptions ~= { _ filterNot { _ == "-Xfatal-warnings" } },
   autoAPIMappings := true
)

lazy val publishingSettings = Seq(
  sonatypeCredentialHost := sonatypeCentralHost,
  publishTo := sonatypePublishToBundle.value,
  versionScheme := Some("early-semver"),
  organization := "com.spinoco",
  homepage := Some(url("https://github.com/spinoco/protocol")),
  licenses := List("MIT" -> url("http://opensource.org/licenses/MIT")),
  developers := {
    for ((username, name) <- contributors) yield
      Developer(
        username,
        name,
        "",
        url(s"https://github.com/$username")
      )
  }.toList,
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/spinoco/protocol"),
      "scm:git@github.com:spinoco/protocol.git"
    )
  )
)

lazy val releaseSettings = Seq(
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value
)

lazy val noPublish = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false,
  publish / skip := true,
  publishLocal / skip := true
)


lazy val common =
  project.in(file("common"))
  .settings(commonSettings)
  .settings(
    name := "protocol-common"
  )

lazy val mime =
  project.in(file("mime"))
  .settings(commonSettings)
  .settings(
    name := "protocol-mime"
  )
  .dependsOn(common)

lazy val mail =
  project.in(file("mail"))
  .settings(commonSettings)
  .settings(
    name := "protocol-mail"
  )
  .dependsOn(common, mime)

lazy val rtp =
  project.in(file("rtp"))
  .settings(commonSettings)
  .settings(
    name := "protocol-rtp"
  )
  .dependsOn(common)

lazy val stun =
  project.in(file("stun"))
  .settings(commonSettings)
  .settings(
    name := "protocol-stun"
  )
  .dependsOn(common)


lazy val webSocket =
  project.in(file("websocket"))
  .settings(commonSettings)
  .settings(
    name := "protocol-websocket"
  )
  .dependsOn(common)

lazy val http =
  project.in(file("http"))
  .settings(commonSettings)
  .settings(
    name := "protocol-http"
  )
  .dependsOn(common, mime)

lazy val sdp =
  project.in(file("sdp"))
    .settings(commonSettings)
    .settings(
      name := "protocol-sdp"
    ).dependsOn(common)

lazy val mgcp =
  project.in(file("mgcp"))
    .settings(commonSettings)
    .settings(
      name := "protocol-mgcp"
    ).dependsOn(common, sdp)

lazy val kafka =
  project.in(file("kafka"))
  .settings(commonSettings)
  .settings(
    name := "protocol-kafka"
    , libraryDependencies ++= Seq(
      "org.xerial.snappy" % "snappy-java" % "1.1.8.4"  // Updated for JDK 11 compatibility
    )
  ).dependsOn(
    common
  )

// we need kafka tests to be run only in scala 2.12, as kafka 0.10.2 does not support 2.13+
lazy val kafkaTests =
  project.in(file("kafka-tests"))
  .settings(commonSettings)
  .settings(
    name := "protocol-kafka-tests"
    // Only cross-compile for Scala 2.12, skip other versions for Kafka tests
    , crossScalaVersions := Seq("2.12.20")
    , scalaVersion := "2.12.20"
    , libraryDependencies ++= Seq(
      "org.xerial.snappy" % "snappy-java" % "1.1.8.4"  // Updated for JDK 11 compatibility
      , "org.apache.kafka" %% "kafka" % "0.10.2.0" % "test"
    )
    , Test / fork := true
    // This project only has test sources, no main sources
    , Compile / sources := Seq.empty
    , Compile / resources := Seq.empty
  ).dependsOn(
    kafka,
    common % "test->test"
  )

lazy val asn1 =
  project.in(file("asn1"))
  .settings(commonSettings)
  .settings(
    name := "protocol-asn1"
  ).dependsOn(common)

lazy val ldap =
  project.in(file("ldap"))
  .settings(commonSettings)
  .settings(
    name := "protocol-ldap"
  ).dependsOn(common, asn1)

lazy val allProtocols =
  project.in(file("."))
 .settings(commonSettings)
 .settings(noPublish)
 .aggregate(
   common
   , mime
   , mail
   , stun
   , webSocket, http
   , rtp
   , sdp
   , mgcp
   , kafka
   , asn1
   , ldap
 )
