# UW NLPTools

This is a collection of natural language processing tools wrapped behind common
interfaces.  The client can easily use the wrapped libraries through an
elegant scala interface.  It's also a breeze to switch implementations since
all implementations of a particular tool extend a common trait.

This toolkit also aims to minimize the size of transitive dependencies.  Each
tool is broken into its own component so you can choose what you want to use.
Each component contains the requisite modules but no more, saving you from
searching for models but also protecting you from a dependencies with hundreds
of megabytes of transitive dependencies.  If you want to avoid the default
models, that's OK too.  They are a transitive dependency of the tool so you can
exclude them with a maven exclusion.

Licensing can be a nightmare.  Each tool is split into its own component with
the most permissive license allowable by the dependencies.  Licenses are all
clearly stated in the LICENSE file of the subcomponent.

The largest NLP components are OpenNLP toolkit (Apache 2.0) and Stanford
CoreNLP (GPL 2.0).

The interfaces are defined in the `core` component.

## Components

### Stemmers

* Morpha
* Snowball
  * Porter
  * Porter2
  * Lovins

### Tokenizers

* OpenNLP
* Stanford

### Part-of-Speech (POS) Taggers

* OpenNLP
* Stanford

### Chunkers

* OpenNLP

### Constituency Parsers

* OpenNLP
* Stanford

### Dependency Parsers

* MaltParser
* Stanford

### Coreference

* Stanford

### Sentence Segmentation (sentencer)

* OpenNLP
* Piao

## Usage

Each component is usable through a java interface as well as on the command
line.  For example:

  `echo 'You are a fool to believe that!' | mvn exec:java -Dexec.mainClass=edu.washington.cs.knowitall.tool.parse.StanfordParse`

## Contributors

* Michael Schmitz <http://www.schmitztech.com/>
