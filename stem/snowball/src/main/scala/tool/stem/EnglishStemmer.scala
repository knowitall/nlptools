package edu.washington.cs.knowitall
package tool
package stem

import org.tartarus.snowball.SnowballProgram
import org.tartarus.snowball

/** Also known as the Porter2 stemmer. */
class EnglishStemmer
extends SnowballStemmer(new snowball.ext.EnglishStemmer)

object EnglishStemmer
extends StemmerMain {
  lazy val stemmer = new PorterStemmer
}
