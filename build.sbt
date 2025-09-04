val ReleaseTag = """^release/([\d\.]+a?)$""".r

lazy val contributors = Seq(
 "pchlupacek" -> "Pavel Chlupáček"
  , "mrauilm" -> "Milan Raulim"
  , "eikek" -> "Eike Kettner"
  , "d6y" -> "Richard Dallaway"
)


lazy val commonSettings = Seq(
   organization := "com.spinoco",
   scalaVersion := "2.12.20",
  crossScalaVersions := Seq("2.12.20"),
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
   scalacOptions in (Compile, console) ~= {_.filterNot("-Ywarn-unused-import" == _).filterNot("-Ywarn-value-discard" == _)},
   scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
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
  parallelExecution in Test := false,
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
  publishArtifact in Test := true,
  Test / fork := true
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
  publish := {},
  publishLocal := {},
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
    , crossScalaVersions := Seq("2.12.20")
    , scalaVersion := "2.12.20"
    , libraryDependencies ++= Seq(
      "org.xerial.snappy" % "snappy-java" % "1.1.2.1"  // for supporting a Snappy compression of message sets
      , "org.apache.kafka" %% "kafka" % "0.10.2.0" % "test"
    )
    , Test / javaHome := {
      // Try to find JDK 1.8 in common locations
      val jdk8Paths = Seq(
        Some("/Library/Java/JavaVirtualMachines/jdk1.8.0_202.jdk/Contents/Home"),
        Some("/Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home"),
        Some("/usr/lib/jvm/java-8-openjdk"),
        Some("/usr/lib/jvm/java-1.8.0-openjdk"),
        sys.env.get("JAVA_8_HOME")
      ).flatten
      
      jdk8Paths.find(path => new java.io.File(path).exists()) match {
        case Some(jdk8Path) => 
          println(s"Using JDK 1.8 for Kafka tests: $jdk8Path")
          Some(file(jdk8Path))
        case None => 
          println("Warning: JDK 1.8 not found, using system default for Kafka tests")
          None
      }
    }
    , Test / javaOptions := Seq() // Clear the Java 9+ options for this project
    , javacOptions ++= Seq("-source", "1.8", "-target", "1.8")
    , scalacOptions ++= Seq("-target:jvm-1.8")
  ).dependsOn(
    common
    , common % "test->test"
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
