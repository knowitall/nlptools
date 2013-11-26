package edu.knowitall.tool.wordnet

import edu.mit.jwi.Dictionary
import edu.mit.jwi.item.{ POS, Pointer, ISynsetID, ISynset }
import edu.mit.jwi.morph.WordnetStemmer

import scala.collection.JavaConverters._
import scala.collection.immutable.HashSet

import edu.knowitall.tool.postag.PostaggedToken

/**
  * This class holds a few methods that allow for easy access to the WordNet
  * database using the JWI library.
  */
class JwiTools(dict: Dictionary) {

  /** Stemmer is a JWI WordnetStemmer object. */
  val stemmer = new WordnetStemmer(dict)

  val nounSubjectBlacklist: Set[String] = HashSet("it", "he", "she", "they")

  /**
    * Finds the hypernyms for the nth sense of the term represented by the
    * given sequence of PostaggedTokens
    */
  def posTokensToHypernymStream(posTokens: Seq[PostaggedToken], n: Int): Stream[Set[ISynset]] = {

    /* Takes a sequence of POS-tagged tokens and finds the subject noun.
     * 
     * If tokens contains a cardinal number CD (e.g., one, 21) and no nouns,
     *   CD is assumed to be the subject. 
     * Given tokens containing a single noun n, returns n.
     * Given tokens containing multiple CONSECUTIVE nouns, attempts to find
     *   the entire String in WordNet. On failure, chops off the first noun
     *   and tries again until success.
     * Given a sequence containing multiple NONCONSECUTIVE nouns, returns
     *   the largest noun subject prior to any non-"of" prepositions but
     *   after all adjectives. 
     * 
     * @param tokens a sequence of POS-tagged tokens containing a subject noun
     *               that can be found in WordNet.
     * @returns the subject noun as a string. 
     * 
     */
    def getNounSubject(tokens: Seq[PostaggedToken]): String = {

      /* Returns the argument sequence with only nouns, cardinal numbers, 
       * tokens with the POS tag (apostrophes), and the preposition "of". 
       * Anything after any other preposition is discarded. 
       */
      def preprocessTokens(toProcess: Seq[PostaggedToken]): Seq[PostaggedToken] = {

        // remove everything after some non-"of" preposition
        val indexOfNonOfPrep = toProcess.toList.indexWhere(t =>
          t.postag == "IN" && t.string != "of")
        val noNonOfPrep = if (indexOfNonOfPrep == -1) toProcess
        else toProcess.take(indexOfNonOfPrep)

        // remove everything but nouns, ofs, and 's
        val nounCDPrepPosOnly = noNonOfPrep.filter(t =>
          t.isNoun || t.postag == "POS" || t.postag == "IN" || t.postag == "CD")

        return nounCDPrepPosOnly
      }

      /* Attempts to find a WordNet noun in the sequence of tokens given. 
       * 
       * @requires toSearch to have been passed through preprocessTokens before
       *           use - must only have nouns, CDs, "of"s, and POS. 
       * @param toSearch a sequence of PostaggedTokens to search for a WordNet 
       *        noun. 
       * @returns the longest noun in toSearch that is in WordNet. 
       */
      def findNoun(toSearch: Seq[PostaggedToken]): String = {
        if (toSearch == Nil) return ""
        // search for word in WordNet
        val strings = toSearch.map(token => token.string)
        val stemmedWord = stem(strings.mkString(" "), n)

        if (stemmedWord == "") {
          // was not found by stemmer. if toSearch is a single noun n followed
          // by "of" and more tokens, search WordNet for n; else search WordNet
          // for toSearch's tail. 
          if (toSearch.length > 1 && toSearch(1).postag == "IN") {
            findNoun(List(toSearch(0)))
          } else {
            findNoun(toSearch.tail)
          }
        } else {
          // look for stemmedWord in WordNet
          val idxWord = dict.getIndexWord(stemmedWord, POS.NOUN)

          if (idxWord != null && !nounSubjectBlacklist.contains(stemmedWord)) {

            stemmedWord

          } else {

            // word not found in WordNet. do the same check for "of"
            // described above
            if (toSearch.length > 1 && toSearch(1).postag == "IN") {
              findNoun(List(toSearch(0)))
            } else {
              findNoun(toSearch.tail)
            }
          }
        }
      }
      findNoun(preprocessTokens(tokens))
    }

    val nounSubject: String = getNounSubject(posTokens)

    return if (nounSubject == "") Stream(Set())
    else stringToHypernymStream(nounSubject, n)
  }

  /**
    * Goes from a string word to a hypernym stream of the word's nth noun sense.
    *
    * For example, inputting "dog" and 0 would return a stream where the 0th
    * index would hold the synset s1 for the first meaning of "dog", the 1st index
    * would hold the hypernyms s2 of s1, the 2nd index would hold the hypernyms
    * of s2, and so on.
    *
    * @requires str exists in WordNet with an nth noun sense.
    * @param str the noun to look up in WordNet
    * @param n the sense to look up
    * @returns a Stream of Sets of Synsets i where each i is a hypernym
    *          of a ISynset in the argument Set, and each level in the
    *          stream returns one deeper level in the hypernym hierarchy.
    */
  def stringToHypernymStream(str: String, n: Int): Stream[Set[ISynset]] = {
    val stemmedStr = stem(str, 0)
    val nthSynset = stringToNthSynset(stemmedStr, n)
    val hypernyms = synsetToHypernyms(nthSynset)

    hypernymStream(hypernyms)
  }

  /**
    * Gets a stream of hypernym sets.
    *
    * @param synsets a Set of Synsets.
    * @returns a Stream of Set of Synsets where each Synset is a hypernym
    *          of a Synset in the argument set, and each level in the
    *          stream returns one deeper level in the hypernym hierarchy.
    */
  def hypernymStream(synsets: Set[ISynset]): Stream[Set[ISynset]] = {
    if (synsets.size == 0) {
      Stream.Empty
    } else {
      val hypernyms = synsets flatMap synsetToHypernyms
      synsets #:: hypernymStream(hypernyms)
    }
  }

  /**
    * Goes from a synset to a Set of its hypernyms (which are
    * also synsets).
    *
    * @param synset the synset to get hypernyms for.
    */
  def synsetToHypernyms(synset: ISynset): Set[ISynset] = {
    val hypernymIDs = synset.getRelatedSynsets(Pointer.HYPERNYM).asScala.toSet

    // map from jwi synsets to synsets
    hypernymIDs.map(sid => dict.getSynset(sid))
  }

  /**
    * Goes from a string word to the synset of its nth noun sense.
    *
    * @requires str is a noun in WordNet with an nth sense.
    * @param str the word to query in Wordnet.
    * @param n the sense to look up.
    * @return the synset of str's nth sense.
    */
  def stringToNthSynset(str: String, n: Int): ISynset = {
    val strStem = stem(str, 0)
    val idxWord = dict.getIndexWord(strStem, POS.NOUN)
    val wordID = idxWord.getWordIDs.get(n)
    val wnWord = dict.getWord(wordID)

    wnWord.getSynset
  }

  /**
    * Find a word's corresponding WordNet stem.
    *
    * @requires word must be a noun in the WordNet dictionary.
    * @param word the word to find the stem of.
    * @param n which stem to return - set this to 0, rarely will you want others.
    * @return If word's nth WordNet stem exists, returns it; else empty string.
    */
  def stem(word: String, n: Int = 0): String = {
    val stems = stemmer.findStems(word, POS.NOUN).asScala
    return if (stems.length > n) stems(n) else ""
  }
}
