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
line.  Here are some examples:

  `echo 'The quick brown fox jumps over the lazy dog.' | sbt 'project nlptools-parse-stanford' 'run-main edu.knowitall.tool.parse.StanfordParserMain'`
  `echo 'The quick brown fox jumps over the lazy dog.' | sbt 'project nlptools-chunk-opennlp' 'run-main edu.knowitall.tool.chunk.OpenNlpChunkerMain'`

## HTTP

You can also spin each tool up as an HTTP server.

  `sbt 'project nlptools-parse-stanford' 'run-main edu.knowitall.tool.parse.StanfordParserMain --server'`
  
It's also possible to spin up all nlptools components as HTTP servers.

```
$ scala scripts/assignports.scala > servers.config
$ scala scripts/runservers.scala servers.config
```

Inconveniently, this spins up many programs on different ports that can receive POST and GET requests.
Remembering which port corresponds to which tool is confusing at best.  There is a simple scala 
application in `/server` that runs yet another server to unify the tools.  Once you run the nlptools
server you can post to the subpath corresponding to your tool.

```
sbt 'run ../servers.config --port 12000'
```

Now you can POST to a convenient URL.

```
$ curl localhost:12000
/postag/clear/
/postag/opennlp/
/postag/stanford/
/tokenize/clear/
/tokenize/breeze/
/tokenize/opennlp/
/tokenize/stanford/
```

```
$ curl localhost:12000/postag/opennlp/ --data-binary 'Let us postag this text.'
Let 0 VB
us 4 PRP
postag 7 VB
this 14 DT
text 19 NN
. 23 .
```

Or from stdin:

```
echo "Let us postag this text." | curl -X POST --data-binary @- localhost:12000/postag/opennlp/
Let 0 VB
us 4 PRP
postag 7 VB
this 14 DT
text 19 NN
. 23 .
```


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
