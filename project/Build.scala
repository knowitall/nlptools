import sbt._
import Keys._
object NlpToolsBuild extends Build {
  lazy val root = Project(id = "nlptools", base = file(".")) aggregate(core,
    opennlpSentence, opennlpTokenize, opennlpPostag, opennlpChunk, opennlpParse,
    stanfordTokenize, stanfordPostag, stanfordParse, stanfordCoref,
    clearTokenize, clearPostag, clearParse, clearSrl,
    breezeTokenize, breezeSentence, breezeConf,
    morphaStemmer, snowballStemmer)

  val opennlp = "org.apache.opennlp" % "opennlp-tools" % "1.5.2-incubating"

  val stanfordModelGroup = "edu.washington.cs.knowitall.stanford-corenlp"
  val stanfordVersion = "1.3.4"
  val stanford = "edu.stanford.nlp" % "stanford-corenlp" % stanfordVersion

  val clearModelGroup = "edu.washington.cs.knowitall.clearnlp"
  val clearVersion = "1.3.0"
  val clear = "com.googlecode.clearnlp" % "clearnlp" % clearVersion

  val buildOrganization = "edu.knowitall.nlptools"
  val buildVersion = "2.4.0-SNAPSHOT"
  val buildScalaVersion = "2.9.2"
  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion
  )

  val junit = "junit" % "junit" % "4.11"
  val commonScala = "edu.washington.cs.knowitall.common-scala" %% "common-scala" % "1.0.9"
  val specs2 = "org.specs2" %% "specs2" % "1.12.3"
  val scopt = "com.github.scopt" %% "scopt" % "2.1.0"
  val slf4j = "org.slf4j" % "slf4j-api" % "1.7.2"
  val unfilteredFilter = "net.databinder" %% "unfiltered-filter" % "0.6.5"
  val unfilteredJetty = "net.databinder" %% "unfiltered-jetty" % "0.6.5"

  lazy val core = Project(id = "nlptools-core", base = file("core"), settings = buildSettings ++ Seq(
    libraryDependencies ++= Seq(junit, commonScala, scopt, slf4j, specs2, unfilteredFilter, unfilteredJetty)
  ))

  // OpenNLP

  lazy val opennlpSentence = Project(
    id = "nlptools-sentence-opennlp",
    base = file("sentence/opennlp"),
    settings = buildSettings ++ Seq(
      (libraryDependencies ++= Seq(opennlp, "edu.washington.cs.knowitall" % "opennlp-sent-models" % "1.5" )))
  ) dependsOn(core)

  lazy val opennlpTokenize = Project(
    id = "nlptools-tokenize-opennlp",
    base = file("tokenize/opennlp"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(opennlp, "edu.washington.cs.knowitall" % "opennlp-tokenize-models" % "1.5" ))
  ) dependsOn(core)


  lazy val opennlpPostag = Project(
    id = "nlptools-postag-opennlp",
    base = file("postag/opennlp"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(opennlp, "edu.washington.cs.knowitall" % "opennlp-postag-models" % "1.5" ))
  ) dependsOn(opennlpTokenize)

  lazy val opennlpChunk = Project(
    id = "nlptools-chunk-opennlp",
    base = file("chunk/opennlp"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(opennlp, "edu.washington.cs.knowitall" % "opennlp-chunk-models" % "1.5" ))
  ) dependsOn(opennlpPostag)

  lazy val opennlpParse = Project(
    id = "nlptools-parse-opennlp",
    base = file("parse/opennlp"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(opennlp, "edu.washington.cs.knowitall" % "opennlp-parse-models" % "1.5" ))
  ) dependsOn(opennlpPostag)

  // Stanford

  lazy val stanfordTokenize = Project(
    id = "nlptools-tokenize-stanford",
    base = file("tokenize/stanford"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(stanford))
  ) dependsOn(core)

  lazy val stanfordPostag = Project(
    id = "nlptools-postag-stanford",
    base = file("postag/stanford"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(stanford, stanfordModelGroup % "stanford-postag-models" % stanfordVersion ))
  ) dependsOn(stanfordTokenize)

  lazy val stanfordParse = Project(
    id = "nlptools-parse-stanford",
    base = file("parse/stanford"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(stanford, stanfordModelGroup % "stanford-parse-models" % stanfordVersion ))
  ) dependsOn(stanfordPostag)

  lazy val stanfordCoref = Project(
    id = "nlptools-coref-stanford",
    base = file("coref/stanford"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(stanford,
        stanfordModelGroup % "stanford-sutime-models" % stanfordVersion ,
        stanfordModelGroup % "stanford-ner-models" % stanfordVersion ,
        stanfordModelGroup % "stanford-dcoref-models" % stanfordVersion, scopt ))
  ) dependsOn(stanfordParse)

/*
  lazy val stanfordTyper = Project(
    id = "nlptools-typer-stanford",
    base = file("typer/stanford"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(stanford, stanfordModelGroup % "stanford-ner-models" % stanfordVersion ))
  ) dependsOn(core)
*/

  // Clear

  lazy val clearTokenize = Project(
    id = "nlptools-tokenize-clear",
    base = file("tokenize/clear"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(clear))
  ) dependsOn(core)

  lazy val clearPostag = Project(
    id = "nlptools-postag-clear",
    base = file("postag/clear"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(clear, clearModelGroup % "clear-postag-models" % clearVersion ))
  ) dependsOn(clearTokenize)

  lazy val clearParse = Project(
    id = "nlptools-parse-clear",
    base = file("parse/clear"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(clear, clearModelGroup % "clear-parse-models" % clearVersion ))
  ) dependsOn(clearPostag)

  lazy val clearSrl = Project(
    id = "nlptools-srl-clear",
    base = file("srl/clear"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(
        clear,
        clearModelGroup % "clear-role-models" % clearVersion,
        clearModelGroup % "clear-pred-models" % clearVersion,
        clearModelGroup % "clear-srl-models" % clearVersion))
  ) dependsOn(clearParse)

  // Breeze

  lazy val breezeTokenize = Project(
    id = "nlptools-tokenize-breeze",
    base = file("tokenize/breeze"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(clear,
        "org.scalanlp" %% "breeze-process" % "0.1"))
  ) dependsOn(core)

  lazy val breezeSentence = Project(
    id = "nlptools-sentence-breeze",
    base = file("sentence/breeze"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(clear,
        "org.scalanlp" %% "breeze-process" % "0.1"))
  ) dependsOn(core)

  lazy val breezeConf = Project(
    id = "nlptools-conf-breeze",
    base = file("conf/breeze"),
    settings = buildSettings ++ Seq(
    libraryDependencies ++= Seq(clear,
      "org.scalanlp" %% "breeze-process" % "0.1",
      "org.scalanlp" %% "breeze-learn" % "0.1"))
  ) dependsOn(core)

  // Stemmers

  lazy val morphaStemmer = Project(
    id = "nlptools-stem-morpha",
    base = file("stem/morpha"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(clear,
        "edu.washington.cs.knowitall" % "morpha-stemmer" % "1.0.4"))
  ) dependsOn(core)

  lazy val snowballStemmer = Project(
    id = "nlptools-stem-snowball",
    base = file("stem/snowball"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(clear,
        "org.apache.lucene" % "lucene-snowball" % "3.0.3"))
  ) dependsOn(core)
}
