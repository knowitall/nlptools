# UW NLPTools

This is a collection of natural language processing tools wrapped behind common
interfaces.  The client can easily use the wrapped libraries through elegant
scala interfaces.  It's also simple to switch implementations of a particular
tool since all implementations of a particular tool extend a common interface.

This toolkit also aims to minimize the size of transitive dependencies.  Each
tool is broken into its own component so you can choose what you want to use
through dependency management.  Each component contains the requisite modules
but no more, saving you from needing to search for models while also protecting
you from a dependencies that are hundreds of megabytes in order to contain
every possible model.  If you want to avoid the default models, that's OK too.
They are a transitive dependency of the tool so you can exclude them within
your dependency manager.

Licensing can be a nightmare.  Each tool is split into its own component with
the most permissive license allowable by the dependencies.  Licenses are all
clearly stated in the LICENSE file of the subcomponent.

The largest NLP components are OpenNLP toolkit (Apache 2.0) and Stanford
CoreNLP (GPL 2.0).

The interfaces are defined in the `core` component.

## Usage

Each component is usable through a java interface as well as on the command
line.  For example:

  `echo 'You are a fool to believe that!' | sbt 'project nlptools-parse-stanford' 'run-main edu.knowitall.tool.parse.StanfordParser`

You can also spin each tool up as a server.

  `sbt 'project nlptools-parse-stanford' 'run-main edu.knowitall.tool.parse.StanfordParser --server'`

## Components

### Stemmers

* Morpha
* Snowball
  * Porter
  * Porter2
  * Lovins

### Tokenizers

* OpenNLP
* Penn Treebank
* Stanford

### Part-of-Speech (POS) Taggers

* Clear
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
* ClearParser

### Coreference

* Stanford

### Sentence Segmentation (sentencer)

* OpenNLP
* Piao

## Contributors

* Michael Schmitz <http://www.schmitztech.com/>
