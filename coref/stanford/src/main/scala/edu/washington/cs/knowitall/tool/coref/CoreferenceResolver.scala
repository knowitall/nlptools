package edu.washington.cs.knowitall
package tool
package coref

/*
 * A coreference resolver takes text as input and produces clusters
 * of mentions with the same target or resolves mentions with the
 * most informative mention. */
abstract class CoreferenceResolver {
  /*
   * Process a document and return a map of mentions in a cluster, where the
   * key is the most representitive mention. */
  def clusters(text: String): Map[String, List[Mention]]

  /*
   * Process a document and return a map of mention resolutions, where the
   * key set covers all mentions and the value is the most representitive
   * mention. */
  def mentions(text: String): Map[Mention, String] = {
    for {
      (bestMention, mentions) <- clusters(text)
      mention <- mentions
    } yield (mention -> bestMention)
  }

  /*
   * Process a document and return a document with all mentions replaced by
   * the most representitive mention. */
  def resolve(text: String, transform: (String,String)=>String = (_, x)=>x): String
}

/*
 * A representation for a mention in a document. */
case class Mention(text: String, offset: Int) {
  override def toString = offset + ":" + "\"" + text + "\""
}
