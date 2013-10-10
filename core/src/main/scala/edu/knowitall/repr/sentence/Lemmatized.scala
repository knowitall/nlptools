package edu.knowitall.repr.sentence

import edu.knowitall.tool.stem._

trait Lemmatized {
  tokenized: TokenizedSupertrait =>

  def lemmatizedTokens: Seq[edu.knowitall.tool.stem.Lemmatized[token]]
}

trait Lemmatizer extends Lemmatized {
  tokenized: TokenizedSupertrait =>

  def lemmatizer: Stemmer

  def postLemmatize(tokens: Seq[edu.knowitall.tool.stem.Lemmatized[token]]):
    Seq[edu.knowitall.tool.stem.Lemmatized[token]] = tokens

  override lazy val lemmatizedTokens: Seq[edu.knowitall.tool.stem.Lemmatized[token]] =
    tokenized.tokens map lemmatizer.lemmatizeToken
}
