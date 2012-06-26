package edu.washington.cs.knowitall
package tool
package stem

import org.tartarus.snowball.SnowballProgram
import org.tartarus.snowball

class PorterStemmer
extends SnowballStemmer(new snowball.ext.PorterStemmer)

object PorterStemmer
extends StemmerMain {
  lazy val stemmer = new PorterStemmer

  implicit def instance: Stemmer = new PorterStemmer
}
