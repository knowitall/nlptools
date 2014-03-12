import sbt._
import Keys._

import sbtassembly.Plugin._
import AssemblyKeys._

import sbtrelease._
import ReleasePlugin._
import ReleaseKeys._
import ReleaseStateTransformations._
import Utilities._

import com.typesafe.sbt.SbtPgp.PgpKeys._

object NlpToolsBuild extends Build {
  // settings
  val buildOrganization = "org.allenai.nlptools"
  val buildScalaVersions = Seq("2.10.3")

  lazy val root = Project(id = "nlptools", base = file(".")).settings (
    crossScalaVersions := buildScalaVersions,
    publish := { },
    publishTo := Some("bogus" at "http://nowhere.com"),
    publishLocal := { }).settings(ReleaseSettings.defaults: _*).aggregate(core,
      opennlpSentence, opennlpTokenize, opennlpPostag, opennlpChunk, opennlpParse,
      stanfordTokenize, stanfordPostag, stanfordParse, stanfordCoref, stanfordTyper,
      maltParse,
      clearTokenize, clearPostag, clearParse, clearSrl,
      breezeTokenize, breezeSentence, breezeConf,
      wekaConf,
      morphaStemmer, snowballStemmer,
      uwHeadword, uwWordnet)

  override lazy val settings = super.settings ++ Seq(
    crossScalaVersions := buildScalaVersions
  )


  // license helpers
  val apache2 = "Apache 2.0 " -> url("http://www.opensource.org/licenses/bsd-3-clause")
  val gpl2 = "GPL 2.0 " -> url("http://www.gnu.org/licenses/gpl-2.0.html")
  val gpl3 = "GPL 3.0 " -> url("http://www.gnu.org/licenses/gpl-3.0.html")

  // dependency helpers
  val opennlp = "org.apache.opennlp" % "opennlp-tools" % "1.5.3" exclude("net.sf.jwordnet", "jwnl")

  val weka = "nz.ac.waikato.cms.weka" % "weka-dev" % "3.7.9"

  val stanfordVersion = "3.3.0"
  val stanford = "edu.stanford.nlp" % "stanford-corenlp" % stanfordVersion
  val stanfordModels = "edu.stanford.nlp" % "stanford-corenlp" % stanfordVersion classifier("models")

  val clearGroup = "com.clearnlp"
  val clearVersion = "2.0.0"
  val clear = clearGroup % "clearnlp" % clearVersion

  val breezeVersion = "0.2"
  val breezeLearn = "org.scalanlp" %% "breeze-learn" % breezeVersion exclude("com.codecommit", "anti-xml_2.9.1") cross CrossVersion.binaryMapped {
          case "2.9.3" => "2.9.2"
          case "2.10.2" => "2.10"
          case x => x
        }
  val breezeProcess = "org.scalanlp" %% "breeze-process" % breezeVersion exclude("com.codecommit", "anti-xml_2.9.1") cross CrossVersion.binaryMapped {
          case "2.9.3" => "2.9.2"
          case "2.10.2" => "2.10"
          case x => x
        }


  // dependencies
  val junit = "junit" % "junit" % "4.11"
  val commonScala = "edu.washington.cs.knowitall.common-scala" %% "common-scala" % "1.1.2"
  val specs2 = "org.specs2" % "specs2" % "1.12.3" cross CrossVersion.binaryMapped {
    case "2.9.3" => "2.9.2"
    case "2.10.2" => "2.10"
    case x => x
  }

  val scopt = "com.github.scopt" % "scopt" % "2.1.0" cross CrossVersion.binaryMapped {
    case "2.9.3" => "2.9.2"
    case "2.10.2" => "2.10"
    case x => x
  }

  val slf4j = "org.slf4j" % "slf4j-api" % "1.7.3"
  val unfilteredFilter = "net.databinder" %% "unfiltered-filter" % "0.7.0"
  val unfilteredJetty = "net.databinder" %% "unfiltered-jetty" % "0.7.0"
  val dispatch = "net.databinder.dispatch" %% "dispatch-core" % "0.11.0"

  val logbackVersion = "1.0.13"
  val logbackCore = "ch.qos.logback" % "logback-core" % logbackVersion
  val logbackClassic = "ch.qos.logback" % "logback-classic" % logbackVersion
  val loggingImpls = Seq(logbackCore, logbackClassic)

  lazy val publishSignedAction = { st: State =>
    val extracted = st.extract
    val ref = extracted.get(thisProjectRef)
    extracted.runAggregated(publishSigned in Global in ref, st)
  }

  // parent build definition
  val buildSettings = Defaults.defaultSettings ++ assemblySettings ++ Format.settings ++ Seq (
    organization := buildOrganization,
    crossScalaVersions := buildScalaVersions,
    scalaVersion <<= (crossScalaVersions) { versions => versions.head },
    libraryDependencies ++= Seq(junit % "test", specs2 % "test",
      dispatch, unfilteredFilter, unfilteredJetty) ++ loggingImpls,
    scalacOptions ++= Seq("-unchecked", "-deprecation"),
    parallelExecution in ThisBuild := false,
    javaOptions += "-Xms512M",
    javaOptions += "-Xmx2G",
    javaOptions += "-XX:MaxPermSize=512M",
    javaOptions += "-XX:ReservedCodeCacheSize=512M",
    fork in test := true,
    publishMavenStyle := true,
    publishTo <<= version { (v: String) =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
    homepage := Some(url("https://github.com/knowitall/nlptools")),
    mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) => {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case _ => MergeStrategy.first
    }},
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      publishArtifacts.copy(action = publishSignedAction),
      setNextVersion,
      commitNextVersion
    ),
    pomExtra := (
      <scm>
        <url>https://github.com/knowitall/nlptools</url>
        <connection>scm:git://github.com/knowitall/nlptools.git</connection>
        <developerConnection>scm:git:git@github.com:knowitall/nlptools.git</developerConnection>
        <tag>HEAD</tag>
      </scm>
      <developers>
       <developer>
          <name>Michael Schmitz</name>
        </developer>
      </developers>))

  // Core

  lazy val core = Project(id = "core", base = file("core"), settings = buildSettings ++ Seq(
    licenses := Seq(apache2),
    libraryDependencies ++= Seq(commonScala, scopt, slf4j)
  ))

  // Headwords

  lazy val uwHeadword = Project(
    id = "headword-uw",
    base = file("headword/uw"),
    settings = buildSettings ++ Seq(
      name := "nlptools-headword-uw",
      licenses := Seq(apache2))
  ) dependsOn(uwWordnet)

  // Wordnet

  lazy val uwWordnet = Project(
    id = "wordnet-uw",
    base = file("wordnet/uw"),
    settings = buildSettings ++ Seq(
      name := "nlptools-wordnet-uw",
      licenses := Seq("MIT Java Wordnet Interface License" -> url("http://projects.csail.mit.edu/jwi/license.html")),
      libraryDependencies ++= Seq("edu.mit" % "jwi" % "2.2.3"))
  ) dependsOn(core)

  // OpenNLP

  lazy val opennlpSentence = Project(
    id = "sentence-opennlp",
    base = file("sentence/opennlp"),
    settings = buildSettings ++ Seq(
      name := "nlptools-sentence-opennlp",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(opennlp, "edu.washington.cs.knowitall" % "opennlp-sent-models" % "1.5" ))
  ) dependsOn(core)

  lazy val opennlpTokenize = Project(
    id = "tokenize-opennlp",
    base = file("tokenize/opennlp"),
    settings = buildSettings ++ Seq(
      name := "nlptools-tokenize-opennlp",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(opennlp, "edu.washington.cs.knowitall" % "opennlp-tokenize-models" % "1.5" ))
  ) dependsOn(core)


  lazy val opennlpPostag = Project(
    id = "postag-opennlp",
    base = file("postag/opennlp"),
    settings = buildSettings ++ Seq(
      name := "nlptools-postag-opennlp",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(opennlp, "edu.washington.cs.knowitall" % "opennlp-postag-models" % "1.5" ))
  ) dependsOn(opennlpTokenize)

  lazy val opennlpChunk = Project(
    id = "chunk-opennlp",
    base = file("chunk/opennlp"),
    settings = buildSettings ++ Seq(
      name := "nlptools-chunk-opennlp",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(opennlp, "edu.washington.cs.knowitall" % "opennlp-chunk-models" % "1.5" ))
  ) dependsOn(opennlpPostag)

  lazy val opennlpParse = Project(
    id = "parse-opennlp",
    base = file("parse/opennlp"),
    settings = buildSettings ++ Seq(
      name := "nlptools-parse-opennlp",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(opennlp, "edu.washington.cs.knowitall" % "opennlp-parse-models" % "1.5" ))
  ) dependsOn(opennlpPostag)

  // Stanford

  lazy val stanfordTokenize = Project(
    id = "tokenize-stanford",
    base = file("tokenize/stanford"),
    settings = buildSettings ++ Seq(
      name := "nlptools-tokenize-stanford",
      licenses := Seq(gpl2),
      libraryDependencies ++= Seq(stanford))
  ) dependsOn(core)

  lazy val stanfordPostag = Project(
    id = "postag-stanford",
    base = file("postag/stanford"),
    settings = buildSettings ++ Seq(
      name := "nlptools-postag-stanford",
      licenses := Seq(gpl2),
      libraryDependencies ++= Seq(stanford, stanfordModels))
  ) dependsOn(stanfordTokenize)

  lazy val stanfordParse = Project(
    id = "parse-stanford",
    base = file("parse/stanford"),
    settings = buildSettings ++ Seq(
      name := "nlptools-parse-stanford",
      licenses := Seq(gpl2),
      libraryDependencies ++= Seq(stanford, stanfordModels),
      mainClass in assembly := Some("edu.knowitall.tool.parse.StanfordParserMain"))
  ) dependsOn(stanfordPostag)

  lazy val stanfordCoref = Project(
    id = "coref-stanford",
    base = file("coref/stanford"),
    settings = buildSettings ++ Seq(
      name := "nlptools-coref-stanford",
      licenses := Seq(gpl2),
      libraryDependencies ++= Seq(stanford,
        stanfordModels,
        scopt ))
  ) dependsOn(stanfordParse)

  lazy val stanfordTyper = Project(
    id = "typer-stanford",
    base = file("typer/stanford"),
    settings = buildSettings ++ Seq(
      name := "nlptools-typer-stanford",
      licenses := Seq(gpl2),
      libraryDependencies ++= Seq(stanford, stanfordModels))
  ) dependsOn(core)

  // Malt

  lazy val maltParse = Project(
    id = "parse-malt",
    base = file("parse/malt"),
    settings = buildSettings ++ Seq(
      name := "nlptools-parse-malt",
      licenses := Seq("Malt Parser License" -> url("http://www.maltparser.org/license.html")),
      libraryDependencies ++= Seq("org.maltparser" % "maltparser" % "1.7.2"))
  ) dependsOn(morphaStemmer, opennlpPostag)

  // Clear

  lazy val clearTokenize = Project(
    id = "tokenize-clear",
    base = file("tokenize/clear"),
    settings = buildSettings ++ Seq(
      name := "nlptools-tokenize-clear",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(clear, clearGroup % "clearnlp-dictionary" % "1.0"))
  ) dependsOn(core)

  lazy val clearPostag = Project(
    id = "postag-clear",
    base = file("postag/clear"),
    settings = buildSettings ++ Seq(
      name := "nlptools-postag-clear",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(clear, clearGroup % "clearnlp-general-en-pos" % "1.0" ))
  ) dependsOn(clearTokenize)

  lazy val clearParse = Project(
    id = "parse-clear",
    base = file("parse/clear"),
    settings = buildSettings ++ Seq(
      name := "nlptools-parse-clear",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(clear, clearGroup % "clearnlp-general-en-dep" % "1.1" ))
  ) dependsOn(clearPostag)

  lazy val clearSrl = Project(
    id = "srl-clear",
    base = file("srl/clear"),
    settings = buildSettings ++ Seq(
      name := "nlptools-srl-clear",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(
        clear,
        clearGroup % "clearnlp-general-en-srl" % "1.0"))
  ) dependsOn(clearParse)

  // Breeze

  lazy val breezeTokenize = Project(
    id = "tokenize-breeze",
    base = file("tokenize/breeze"),
    settings = buildSettings ++ Seq(
      name := "nlptools-tokenize-breeze",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(clear, breezeProcess),
      mainClass in assembly := Some("edu.knowitall.tool.tokenize.SimpleEnglishTokenizerMain"))
  ) dependsOn(core)

  lazy val breezeSentence = Project(
    id = "sentence-breeze",
    base = file("sentence/breeze"),
    settings = buildSettings ++ Seq(
      name := "nlptools-sentence-breeze",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(clear, breezeProcess))
  ) dependsOn(core)

  lazy val breezeConf = Project(
    id = "conf-breeze",
    base = file("conf/breeze"),
    settings = buildSettings ++ Seq(
      name := "nlptools-conf-breeze",
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(clear,
        breezeProcess,
        breezeLearn))
  ) dependsOn(core)

  lazy val wekaConf = Project(
    id = "conf-weka",
    base = file("conf/weka"),
    settings = buildSettings ++ Seq(
      name := "nlptools-conf-weka",
      licenses := Seq(gpl3),
      libraryDependencies ++= Seq(weka, specs2, junit))
  ) dependsOn(core)

  // Stemmers

  lazy val morphaStemmer = Project(
    id = "stem-morpha",
    base = file("stem/morpha"),
    settings = buildSettings ++ Seq(
      name := "nlptools-stem-morpha",
      licenses := Seq(
        "Academic License (for original lex files)" -> url("http://www.informatics.sussex.ac.uk/research/groups/nlp/carroll/morph.tar.gz"),
        "Apache 2.0 (for supplemental code)" -> url("http://www.opensource.org/licenses/bsd-3-clause")),
      libraryDependencies ++= Seq(clear,
        "edu.washington.cs.knowitall" % "morpha-stemmer" % "1.0.5"))
  ) dependsOn(core)

  lazy val snowballStemmer = Project(
    id = "stem-snowball",
    base = file("stem/snowball"),
    settings = buildSettings ++ Seq(
      name := "nlptools-stem-snowball",
      licenses := Seq("BSD" -> url("http://snowball.tartarus.org/license.php")),
      libraryDependencies ++= Seq(clear,
        "org.apache.lucene" % "lucene-snowball" % "3.0.3"))
  ) dependsOn(core)
}
