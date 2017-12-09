import com.typesafe.sbt.pgp.PgpKeys.publishSigned

val ReleaseTag = """^release/([\d\.]+a?)$""".r

lazy val contributors = Seq(
 "pchlupacek" -> "Pavel Chlupáček"
  , "mrauilm" -> "Milan Raulim"
  , "eikek" -> "Eike Kettner"
  , "d6y" -> "Richard Dallaway"
)


lazy val commonSettings = Seq(
   organization := "com.spinoco",
   scalaVersion := "2.12.4",
  crossScalaVersions := Seq("2.11.11", "2.12.4"),
   scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
    "-language:postfixOps",
    "-Xfatal-warnings",
    "-Yno-adapted-args",
    "-Ywarn-value-discard",
    "-Ywarn-unused-import"
   ),
   scalacOptions in (Compile, console) ~= {_.filterNot("-Ywarn-unused-import" == _)},
   scalacOptions in (Test, console) <<= (scalacOptions in (Compile, console)),
   libraryDependencies ++= Seq(
     "org.scodec" %% "scodec-bits" % "1.1.5"
     , "org.scodec" %% "scodec-core" % "1.10.3"
     , "org.scalatest" %% "scalatest" % "3.0.0" % "test"
     , "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
   ),
   scmInfo := Some(ScmInfo(url("https://github.com/Spinoco/protocol"), "git@github.com:Spinoco/protocol.git")),
   homepage := None,
   licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
   initialCommands := s"""
  """
) ++ testSettings ++ scaladocSettings ++ publishingSettings ++ releaseSettings

lazy val testSettings = Seq(
  parallelExecution in Test := false,
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
  publishArtifact in Test := true
)

lazy val scaladocSettings = Seq(
   scalacOptions in (Compile, doc) ++= Seq(
    "-doc-source-url", scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath,
    "-implicits",
    "-implicits-show-all"
  ),
   scalacOptions in (Compile, doc) ~= { _ filterNot { _ == "-Xfatal-warnings" } },
   autoAPIMappings := true
)

lazy val publishingSettings = Seq(
  publishTo := {
   val nexus = "https://oss.sonatype.org/"
   if (version.value.trim.endsWith("SNAPSHOT"))
     Some("snapshots" at nexus + "content/repositories/snapshots")
   else
     Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  credentials ++= (for {
   username <- Option(System.getenv().get("SONATYPE_USERNAME"))
   password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
  } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq,
  publishMavenStyle := true,
  pomIncludeRepository := { _ => false },
  pomExtra := {
    <url>https://github.com/Spinoco/protocol</url>
    <developers>
      {for ((username, name) <- contributors) yield
      <developer>
        <id>{username}</id>
        <name>{name}</name>
        <url>http://github.com/{username}</url>
      </developer>
      }
    </developers>
  },
  pomPostProcess := { node =>
   import scala.xml._
   import scala.xml.transform._
   def stripIf(f: Node => Boolean) = new RewriteRule {
     override def transform(n: Node) =
       if (f(n)) NodeSeq.Empty else n
   }
   val stripTestScope = stripIf { n => n.label == "dependency" && (n \ "scope").text == "test" }
   new RuleTransformer(stripTestScope).transform(node)(0)
  }
)

lazy val releaseSettings = Seq(
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value
)

lazy val noPublish = Seq(
  publish := (),
  publishLocal := (),
  publishSigned := (),
  publishArtifact := false
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
      "org.xerial.snappy" % "snappy-java" % "1.1.2.1"  // for supporting a Snappy compression of message sets
      , "org.apache.kafka" %% "kafka" % "0.10.2.0" % "test"
    )
  ).dependsOn(
    common
    , common % "test->test"
  )


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
 )
