addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.1")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.8")

addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.2.1")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "0.6.4")

resolvers ++= Seq(
  "allenai nexus repository" at "http://utility.allenai.org:8081/nexus/content/repositories/releases",
  "allenai nexus repository snapshots" at "http://utility.allenai.org:8081/nexus/content/repositories/snapshots"
)

addSbtPlugin("org.allenai.plugins" % "sbt-deploy" % "2014.2.24-1-SNAPSHOT")
