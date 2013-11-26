package edu.knowitall
package tool
package stem

import org.tartarus.snowball.SnowballProgram
import org.tartarus.snowball

/** Also known as the Porter2 stemmer. */
class EnglishStemmer
  extends SnowballStemmer(new snowball.ext.EnglishStemmer)

object EnglishStemmer {
  implicit def instance: Stemmer = new EnglishStemmer
}

object EnglishStemmerMain
    extends StemmerMain {
  lazy val stemmer = new EnglishStemmer
}
