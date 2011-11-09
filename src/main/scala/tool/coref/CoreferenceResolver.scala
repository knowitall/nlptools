package edu.washington.cs.knowitall
package tool
package coref

abstract class CoreferenceResolver {
  /*
   * Process a document and return a map of mentions, where the key is the
   * most representitive mention. */
  def mentions(text: String): Map[String, List[Mention]]

  /*
   * Process a document and return a document with all mentions replaced by
   * the most representitive mention. */
  def resolve(text: String, transform: (String,String)=>String = (_, x)=>x): String
}

class Mention(text: String, offset: Int) {
  override def toString = offset + ":" + "\"" + text + "\""
}
