package edu.knowitall.repr.sentence

import edu.knowitall.tool.stem._

trait Lemmas {
  tokenized: TokensSupertrait =>

  def lemmatizedTokens: Seq[edu.knowitall.tool.stem.Lemmatized[token]]
}

trait Lemmatizer extends Lemmas {
  tokenized: TokensSupertrait =>

  def lemmatizer: Stemmer

  override lazy val lemmatizedTokens: Seq[edu.knowitall.tool.stem.Lemmatized[token]] =
    tokenized.tokens map lemmatizer.lemmatizeToken
}
