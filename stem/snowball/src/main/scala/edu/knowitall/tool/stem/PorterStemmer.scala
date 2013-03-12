package edu.knowitall
package tool
package stem

import org.tartarus.snowball.SnowballProgram
import org.tartarus.snowball

class PorterStemmer
extends SnowballStemmer(new snowball.ext.PorterStemmer)

object PorterStemmer {
  implicit def instance: Stemmer = new PorterStemmer
}

object PorterStemmerMain
extends StemmerMain {
  lazy val stemmer = new PorterStemmer
}
