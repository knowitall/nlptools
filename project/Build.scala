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
  val buildOrganization = "edu.washington.cs.knowitall.nlptools"
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

  val stanfordModelGroup = "edu.washington.cs.knowitall.stanford-corenlp"
  val stanfordVersion = "1.3.5"
  val stanford = "edu.stanford.nlp" % "stanford-corenlp" % stanfordVersion

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

  lazy val publishSignedAction = { st: State =>
    val extracted = st.extract
    val ref = extracted.get(thisProjectRef)
    extracted.runAggregated(publishSigned in Global in ref, st)
  }

  // parent build definition
  val buildSettings = Defaults.defaultSettings ++ assemblySettings ++ Seq (
    organization := buildOrganization,
    crossScalaVersions := buildScalaVersions,
    scalaVersion <<= (crossScalaVersions) { versions => versions.head },
    libraryDependencies ++= Seq(junit % "test", specs2 % "test",
      dispatch, unfilteredFilter, unfilteredJetty),
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

  lazy val core = Project(id = "nlptools-core", base = file("core"), settings = buildSettings ++ Seq(
    licenses := Seq(apache2),
    libraryDependencies ++= Seq(commonScala, scopt, slf4j)
  ))

  // Headwords

  lazy val uwHeadword = Project(
    id = "nlptools-headword-uw",
    base = file("headword/uw"),
    settings = buildSettings ++ Seq(
      licenses := Seq(apache2))
  ) dependsOn(uwWordnet)

  // Wordnet

  lazy val uwWordnet = Project(
    id = "nlptools-wordnet-uw",
    base = file("wordnet/uw"),
    settings = buildSettings ++ Seq(
      licenses := Seq("MIT Java Wordnet Interface License" -> url("http://projects.csail.mit.edu/jwi/license.html")),
      libraryDependencies ++= Seq("edu.mit" % "jwi" % "2.2.3"))
  ) dependsOn(core)

  // OpenNLP

  lazy val opennlpSentence = Project(
    id = "nlptools-sentence-opennlp",
    base = file("sentence/opennlp"),
    settings = buildSettings ++ Seq(
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(opennlp, "edu.washington.cs.knowitall" % "opennlp-sent-models" % "1.5" ))
  ) dependsOn(core)

  lazy val opennlpTokenize = Project(
    id = "nlptools-tokenize-opennlp",
    base = file("tokenize/opennlp"),
    settings = buildSettings ++ Seq(
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(opennlp, "edu.washington.cs.knowitall" % "opennlp-tokenize-models" % "1.5" ))
  ) dependsOn(core)


  lazy val opennlpPostag = Project(
    id = "nlptools-postag-opennlp",
    base = file("postag/opennlp"),
    settings = buildSettings ++ Seq(
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(opennlp, "edu.washington.cs.knowitall" % "opennlp-postag-models" % "1.5" ))
  ) dependsOn(opennlpTokenize)

  lazy val opennlpChunk = Project(
    id = "nlptools-chunk-opennlp",
    base = file("chunk/opennlp"),
    settings = buildSettings ++ Seq(
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(opennlp, "edu.washington.cs.knowitall" % "opennlp-chunk-models" % "1.5" ))
  ) dependsOn(opennlpPostag)

  lazy val opennlpParse = Project(
    id = "nlptools-parse-opennlp",
    base = file("parse/opennlp"),
    settings = buildSettings ++ Seq(
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(opennlp, "edu.washington.cs.knowitall" % "opennlp-parse-models" % "1.5" ))
  ) dependsOn(opennlpPostag)

  // Stanford

  lazy val stanfordTokenize = Project(
    id = "nlptools-tokenize-stanford",
    base = file("tokenize/stanford"),
    settings = buildSettings ++ Seq(
      licenses := Seq(gpl2),
      libraryDependencies ++= Seq(stanford))
  ) dependsOn(core)

  lazy val stanfordPostag = Project(
    id = "nlptools-postag-stanford",
    base = file("postag/stanford"),
    settings = buildSettings ++ Seq(
      licenses := Seq(gpl2),
      libraryDependencies ++= Seq(stanford, stanfordModelGroup % "stanford-postag-models" % stanfordVersion ))
  ) dependsOn(stanfordTokenize)

  lazy val stanfordParse = Project(
    id = "nlptools-parse-stanford",
    base = file("parse/stanford"),
    settings = buildSettings ++ Seq(
      licenses := Seq(gpl2),
      libraryDependencies ++= Seq(stanford, stanfordModelGroup % "stanford-parse-models" % stanfordVersion),
      mainClass in assembly := Some("edu.knowitall.tool.parse.StanfordParserMain"))
  ) dependsOn(stanfordPostag)

  lazy val stanfordCoref = Project(
    id = "nlptools-coref-stanford",
    base = file("coref/stanford"),
    settings = buildSettings ++ Seq(
      licenses := Seq(gpl2),
      libraryDependencies ++= Seq(stanford,
        stanfordModelGroup % "stanford-sutime-models" % stanfordVersion ,
        stanfordModelGroup % "stanford-ner-models" % stanfordVersion ,
        stanfordModelGroup % "stanford-dcoref-models" % stanfordVersion, scopt ))
  ) dependsOn(stanfordParse)

  lazy val stanfordTyper = Project(
    id = "nlptools-typer-stanford",
    base = file("typer/stanford"),
    settings = buildSettings ++ Seq(
      licenses := Seq(gpl2),
      libraryDependencies ++= Seq(stanford, stanfordModelGroup % "stanford-ner-models" % stanfordVersion ))
  ) dependsOn(core)

  // Malt

  lazy val maltParse = Project(
    id = "nlptools-parse-malt",
    base = file("parse/malt"),
    settings = buildSettings ++ Seq(
      licenses := Seq("Malt Parser License" -> url("http://www.maltparser.org/license.html")),
      libraryDependencies ++= Seq("org.maltparser" % "maltparser" % "1.7.2"))
  ) dependsOn(morphaStemmer, opennlpPostag)

  // Clear

  lazy val clearTokenize = Project(
    id = "nlptools-tokenize-clear",
    base = file("tokenize/clear"),
    settings = buildSettings ++ Seq(
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(clear, clearGroup % "clearnlp-dictionary" % "1.0"))
  ) dependsOn(core)

  lazy val clearPostag = Project(
    id = "nlptools-postag-clear",
    base = file("postag/clear"),
    settings = buildSettings ++ Seq(
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(clear, clearGroup % "clearnlp-general-en-pos" % "1.0" ))
  ) dependsOn(clearTokenize)

  lazy val clearParse = Project(
    id = "nlptools-parse-clear",
    base = file("parse/clear"),
    settings = buildSettings ++ Seq(
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(clear, clearGroup % "clearnlp-general-en-dep" % "1.1" ))
  ) dependsOn(clearPostag)

  lazy val clearSrl = Project(
    id = "nlptools-srl-clear",
    base = file("srl/clear"),
    settings = buildSettings ++ Seq(
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(
        clear,
        clearGroup % "clearnlp-general-en-srl" % "1.0"))
  ) dependsOn(clearParse)

  // Breeze

  lazy val breezeTokenize = Project(
    id = "nlptools-tokenize-breeze",
    base = file("tokenize/breeze"),
    settings = buildSettings ++ Seq(
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(clear, breezeProcess),
      mainClass in assembly := Some("edu.knowitall.tool.tokenize.SimpleEnglishTokenizerMain"))
  ) dependsOn(core)

  lazy val breezeSentence = Project(
    id = "nlptools-sentence-breeze",
    base = file("sentence/breeze"),
    settings = buildSettings ++ Seq(
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(clear, breezeProcess))
  ) dependsOn(core)

  lazy val breezeConf = Project(
    id = "nlptools-conf-breeze",
    base = file("conf/breeze"),
    settings = buildSettings ++ Seq(
      licenses := Seq(apache2),
      libraryDependencies ++= Seq(clear,
        breezeProcess,
        breezeLearn))
  ) dependsOn(core)

  lazy val wekaConf = Project(
    id = "nlptools-conf-weka",
    base = file("conf/weka"),
    settings = buildSettings ++ Seq(
      licenses := Seq(gpl3),
      libraryDependencies ++= Seq(weka, specs2, junit))
  ) dependsOn(core)

  // Stemmers

  lazy val morphaStemmer = Project(
    id = "nlptools-stem-morpha",
    base = file("stem/morpha"),
    settings = buildSettings ++ Seq(
      licenses := Seq(
        "Academic License (for original lex files)" -> url("http://www.informatics.sussex.ac.uk/research/groups/nlp/carroll/morph.tar.gz"),
        "Apache 2.0 (for supplemental code)" -> url("http://www.opensource.org/licenses/bsd-3-clause")),
      libraryDependencies ++= Seq(clear,
        "edu.washington.cs.knowitall" % "morpha-stemmer" % "1.0.5"))
  ) dependsOn(core)

  lazy val snowballStemmer = Project(
    id = "nlptools-stem-snowball",
    base = file("stem/snowball"),
    settings = buildSettings ++ Seq(
      licenses := Seq("BSD" -> url("http://snowball.tartarus.org/license.php")),
      libraryDependencies ++= Seq(clear,
        "org.apache.lucene" % "lucene-snowball" % "3.0.3"))
  ) dependsOn(core)
}
