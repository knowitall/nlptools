package edu.knowitall
package tool
package parse
package graph

import scala.Option.option2Iterable
import scala.collection.immutable
import scala.util.{Try, Success, Failure}
import org.slf4j.LoggerFactory
import edu.knowitall.collection.immutable.Interval
import edu.knowitall.collection.immutable.graph.Direction
import edu.knowitall.collection.immutable.graph.DownEdge
import edu.knowitall.collection.immutable.graph.Graph
import edu.knowitall.collection.immutable.graph.Graph.Edge
import edu.knowitall.collection.immutable.graph.UpEdge
import edu.knowitall.tool.Format
import edu.knowitall.tool.tokenize.Tokenizer
import edu.knowitall.tool.stem.Stemmer
import edu.knowitall.tool.postag.Postagger
import edu.knowitall.tool.tokenize.Token
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.stem.Lemmatized

/** A representation of a graph over dependencies.
  * This richer representation may include the text of the original sentence,
  * the original nodes (before collapsing), and the original dependencies. */
class DependencyGraph(vertices: Set[DependencyNode], edges: Set[Edge[DependencyNode]])
  extends Graph[DependencyNode](vertices, edges) {

  def this(edges: Iterable[Edge[DependencyNode]]) =
    this(edges.flatMap(_.vertices).toSet, edges.toSet)
  
  val nodes = vertices
  val dependencies = edges

  override def toString = DependencyGraph.multilineStringFormat.write(this)

  /** Approximate Stanford's procedure to create collapsed dependencies. */
  def collapse = {
    /** Turn prepositions into edges instead of nodes. */
    def edgifyPrepositions(graph: Graph[DependencyNode]): Graph[DependencyNode] = {
      var g = graph

      // rename prep edges
      g = new Graph[DependencyNode](g.vertices, g.edges.map { e =>
        e.label match {
          case "prep" | "prepc" =>
            val qualifier = if (graph.dedges(e.dest) exists { case DownEdge(e) => e.label == "pcomp" case _ => false }) "c" else ""
            e.copy(label = e.label + qualifier + "_" + e.dest.string.toLowerCase.replaceAll(" ", "_"))
          case _ => e
        }
      })

      // NOTE: conjunctions must be distributed before pobj edges
      // are collapsed.  Otherwise some won't have incoming prep
      // edges to their targets yet.

      // collapse edges (pobj) preceeded by prep
      try {
        g = g.collapse { edge =>
          edge.label == "pobj" && (g.incoming(edge.source) exists (_.label startsWith "prep"))
        } ((nodes: Traversable[DependencyNode]) =>
          nodes.find(n => g.edges(n).exists(e => e.label == "pobj" && e.dest == n)).get
        )
      }
      catch {
        case e: Throwable => DependencyGraph.logger.error("could not collapse pobj.", e)
      }

      // collapse edges (pcomp) preceeded by prep
      try {
        g = g.collapse { edge =>
            edge.label == "pcomp" && (g.incoming(edge.source) exists (_.label startsWith "prep"))
          }( (nodes: Traversable[DependencyNode]) => {
          nodes.find(n => g.edges(n).exists(e => e.label == "pcomp" && e.dest == n)).get
        })
      }
      catch {
        case e: Throwable => DependencyGraph.logger.error("could not collapse pcomp.", e)
      }

      g
    }

    /** Collapse multi-word prepositions into a single node.  This will make illegal nodes. */
    def collapseMultiwordPrepositions(graph: Graph[DependencyNode]): Graph[DependencyNode] = {
      val preps = graph.edges.filter(edge => edge.label == "prep" || edge.label == "pcomp").toList.sortBy(_.dest.id)(Ordering[Int].reverse)

      // follow up prep, advmod, dep, amod edges
      def cond(e: Graph.Edge[DependencyNode]) = e.label == "prep" || e.label == "advmod" || e.label == "dep" || e.label == "amod"

      preps.foldLeft(graph) {
        case (graph, prep) =>
          if (!(graph.edges contains prep)) graph else {
            val last = prep.dest
            val predecessors = graph.vertices.filter(_.id <= last.id).toList.sortBy(_.id)(Ordering[Int].reverse)

            DependencyGraph.reversedSplitMultiwordPrepositions.filter(p => predecessors.map(_.string).startsWith(p)).toSeq match {
              case Seq() => graph
              case mtches =>
                val removeVertices = predecessors.take(mtches.maxBy(_.length).length).drop(1).flatMap(graph.inferiors(_, _.dest != last)).toSet.toList.sorted
                val joinVertices = removeVertices :+ last

                // keep last connected in case we remove some
                // of it's parents
                var parent = last
                while ((joinVertices contains parent) && (graph.indegree(parent) == 1)) {
                  parent = graph.incoming(parent).head.source
                }

                if (joinVertices contains parent) {
                  // we removed parents up to the root--abort
                  graph
                } else {
                  // add an edge from the closest remaining parent
                  // to last, if we need to
                  val extraEdges =
                    if (graph.neighbors(last) contains parent) Nil
                    else List(new Graph.Edge[DependencyNode](parent, last, "prep"))

                  val text = joinVertices.iterator.map(_.string).mkString(" ")
                  new Graph[DependencyNode](
                    extraEdges ++ graph.edges.filterNot(_.vertices exists (removeVertices contains _))).map(vertex =>
                    if (vertex == prep.dest) new DependencyNode(-1, text) // these nodes are only temporary
                    else vertex)
                }
            }
          }
      }
    }

    /** Turn junctions (and / or) into edges instead of nodes. */
    def collapseJunctions(graph: Graph[DependencyNode]) = {
      val conjGraph = graph.edges.filter(edge =>
        // conj edges to a node with no children
        edge.label == "conj" &&
        // source of conj edges has a child cc edge
        graph.dedges(edge.source).exists { case DownEdge(e) => e.label == "cc" case _ => false}
      ).foldLeft(graph) { case (graph, conj) =>
        val ccNodes = graph.dedges(conj.source).filter {
          case DownEdge(e) => e.label == "cc"
          case _ => false
        }.iterator.map(_.edge.dest).toList

        // look left (negative distance) and then right.
        val bestCC = ccNodes.minBy { case cc =>
          val dist = math.abs(cc.id - conj.dest.id)
          if (dist < 0) -ccNodes.length - dist
          else dist
        }

        val newEdges = scala.collection.Set[Edge[DependencyNode]]() ++ graph.edges - conj + conj.copy(label = "conj_"+bestCC.string)

        new Graph[DependencyNode](graph.vertices, newEdges)
      }

      new Graph[DependencyNode](conjGraph.edges filterNot (_.label == "cc"))
    }

    /** Distribute some edges to other nodes connected by conj_and.
      *
      * Incoming/outgoing are defined as a direction relative to the
      * connected component joined by the conjunction.
      *
      * 1.  Distribute nsubj.
      *     a.  "Michael, Rob, and NJ went to Than Vi."
      *     b.  "The apple was crisp and fresh."
      * 2.  Distribute nsubjpass.
      *     a.  incoming: "The bullet and gunpowder was loaded and fired."
      *     b.  outgoing: "The bullet was loaded and fired."
      * 3.  Distribute incoming advmod edges
      *     a.  incoming: "He spoke wisely and surely."
      *     b.  outgoing: "Just write them down and I will edit it for you."
      * 4.  Distribute incoming acomp edges
      *     a.  incoming: "The water looked blue and refreshing.
      * 5.  Distribute incoming amod edges
      *     a.  incoming: "The blue and cool water felt nice."
      *     b.  outgoing: "Pills raise clotting , high blood pressure , heart attack , and stroke . "
      * 6.  Distribute incoming dobj edges
      *     a.  incoming: "Michael found rocks and spiders."
      *     b.  outgoing: "Michael went to the beach and found rocks."
      * 7.  Distribute incoming rcmod edges
      *     a.  incoming: "The woman, who wore a black dress and spoke in the theater, ate cheese."
      *     b.  outgoing:
      * 8.  Distribute incoming ccomp edges
      *     a.  incoming: "He says you swim fast and eat cherries."
      * 9.  Distribute incoming xcomp edges
      *     a.  incoming: "He says you like to swim fast and eat cherries."
      * 10. Distribute incoming prep edges
      *     a.  incoming: "Michael and George went to the beach in Spring and Fall."
      *     b.  outgoing: "Michael and George went to the beach and slept."
      */
    def distributeConjunctions(graph: Graph[DependencyNode]) = {
      // find components connected by conj_and
      val components = graph.components(e => (e.label equalsIgnoreCase "conj_and") || e.label == "conj_&")

      val newEdges = components.flatMap { vertices =>
        val dedges = vertices.flatMap(graph.dedges(_))

        // find new edges needed to distribute conjunction
        for (
          dedge <- dedges;
          if (dedge.edge.label == "nsubj" ||
              dedge.edge.label == "nsubjpass" ||
              dedge.dir == Direction.Down && (
                // distribute "to" in: "I want to swim and eat cherries"
                dedge.edge.label == "aux"
              ) ||
              dedge.dir == Direction.Up && (
                dedge.edge.label == "advmod" ||
                dedge.edge.label == "amod" ||
                dedge.edge.label == "acomp" ||
                dedge.edge.label == "dobj" ||
                dedge.edge.label == "rcmod" ||
                dedge.edge.label == "ccomp" ||
                dedge.edge.label == "xcomp" ||
                (dedge.edge.label startsWith "prep")));
          if !(vertices contains dedge.end);
          v <- vertices;
          newEdge = dedge match {
            case DownEdge(e) => e.copy(source = v)
            case UpEdge(e) => e.copy(dest = v)
          };
          if !(newEdge.source == newEdge.dest);
          if !(graph.edges contains newEdge)
        ) yield (newEdge)
      }

      new Graph[DependencyNode](graph.vertices, graph.edges ++ newEdges)
    }

    val graph = 
        edgifyPrepositions(
            distributeConjunctions(
                collapseJunctions(
                    collapseMultiwordPrepositions(this))))

    new DependencyGraph(graph.vertices, graph.edges)
  }

  /** Simplify xsubj and nsubj to just subj. */
  def collapseXNsubj = {
    val edges = this.edges.map { dep =>
      if ((dep.label equals "xsubj") || (dep.label equals "nsubj"))
        new Edge[DependencyNode](dep.source, dep.dest, "subj")
      else dep
    }
    new DependencyGraph(edges)
  }

  def joined: JoinedDependencyGraph = {
    val joinedNodes = this.vertices map JoinedDependencyNode.from
    val joinedEdges = this.edges map { edge =>
      edge.copy(
          source=JoinedDependencyNode.from(edge.source), 
          dest=JoinedDependencyNode.from(edge.dest))
      }
    new JoinedDependencyGraph(joinedNodes, joinedEdges)
  }
  
  def tokenized(tokens: Seq[Lemmatized[PostaggedToken]]): Graph[TokenDependencyNode] = {
    def from = TokenDependencyNode.from(tokens) _
    val joinedNodes = this.vertices map from
    val joinedEdges = this.edges map { edge =>
      edge.copy(
          source=from(edge.source), 
          dest=from(edge.dest))
      }
    new Graph[TokenDependencyNode](joinedNodes, joinedEdges)
  }
}

object DependencyGraph {
  val logger = LoggerFactory.getLogger(this.getClass)
  
  type JoinedDependencyGraph = Graph[JoinedDependencyNode]
  
  def create[T <: Token](dependencies: Iterable[Dependency]): DependencyGraph = {
    new DependencyGraph(dependencies)
  }
  
  object singlelineStringFormat extends StringFormat("; ")
  object multilineStringFormat extends StringFormat("\n")

  class StringFormat(seperator: String) extends Format[DependencyGraph, String] {
    def write(graph: DependencyGraph) = {
      // serialize tokens on first line
      val pickledDeps = graph.dependencies.iterator map Dependency.stringFormat.write
      pickledDeps.mkString(seperator)
    }

    val nodeRegex = "\\s*\\((.*)\\)\\s*".r
    def read(pickled: String) = {
      val pickledDeps = pickled.split("seperator")
      val deps = pickledDeps map Dependency.stringFormat.read
      DependencyGraph.create(deps)
    }
  }
  
  /*
  class ConllFormat[T <: PostaggedToken](lemmatizer: Stemmer)(implicit tokenSerializer: Format[T, String]) extends Format[DependencyGraph[T], String] {
    def write(graph: DependencyGraph[T]): String = {
      graph.tokens.toSeq.zipWithIndex.map { case (node, index) =>
        val deps = graph.dependencies.filter(_.dest == node)
        require(deps.size <= 1, "multiple dependencies from node: " + node)
        val (destIndex, label) = deps.headOption match {
          case Some(dep) =>
            (dep.source.id, dep.label)
          case None => (0, "root")
        }

        val cols = Iterable(
            index + 1,
            node.string,
            lemmatizer(node.string),
            node.postag,
            "_",
            destIndex,
            label
        )

        cols mkString "\t"
      }.mkString("\n")
    }

    // WARNING: this won't restore the actual sentence text because
    // there is no offset information stored in CONLL format.
    def read(iterator: Iterator[String]): DependencyGraph[T] = {
      val section = iterator.takeWhile(!_.trim.isEmpty).toIndexedSeq

      var offset = 0
      val nodes = section.map { line =>
        val Array(index, string, lemma, postag, _, _, _) = line.split("\t")
        val node = new DependencyNode(string, index.toInt - 1, offset)
        offset += string.length + 1

        node
      }

      val deps = section.flatMap { line =>
        val Array(index, string, lemma, postag, _, sourceIndex, edge) = line.split("\t")
        if (sourceIndex.toInt > 0) {
          Some(new Dependency(nodes(sourceIndex.toInt - 1), nodes(index.toInt - 1), edge))
        }
        else {
          None
        }
      }

      DependencyGraph.create(nodes, deps)
    }

    def read(string: String): DependencyGraph = {
      this.read(string.split("\n").iterator)
    }
  }
  */

  class SerializationException(message: String, cause: Throwable)
  extends RuntimeException(message, cause)

  val reversedSplitMultiwordPrepositions = Postagger.complexPrepositions.map(_.split(" ").toList.reverse)
}
