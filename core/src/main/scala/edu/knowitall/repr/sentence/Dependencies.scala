package edu.knowitall.repr.sentence

import edu.knowitall.tool.tokenize.Token
import edu.knowitall.tool.postag.PostaggedToken
import edu.knowitall.tool.parse._
import edu.knowitall.tool.parse.graph._
import edu.knowitall.collection.immutable.graph.Graph.Edge

trait JustDependencies {
  this: Sentence =>

  def dgraph: DependencyGraph
}

trait Dependencies extends JustDependencies with Postags {
  this: Sentence with Postags =>
}

trait Parser extends Dependencies {
  this: Sentence with Postags =>

  def parser: DependencyParser

  override lazy val dgraph =
    parser.dependencyGraphPostagged(this.tokens)
}

object Dependencies {
  type Sent = Sentence with Dependencies
  object DotWriter {
    def dot(sentence: Sent, title: String): String = {
      val buffer = new StringBuffer(4092)
      printDot(sentence, buffer, title)
      buffer.toString
    }

    def dotWithHighlights(sentence: Sent, title: String, specialNodes: Set[DependencyNode], specialEdges: Set[Edge[DependencyNode]]): String = {
      val buffer = new StringBuffer(4092)
      printDotWithHighlights(sentence, buffer, title, specialNodes, specialEdges)
      buffer.toString
    }

    def dot(sentence: Sent, title: String,
      nodeStyle: Map[DependencyNode, String],
      edgeStyle: Map[Edge[DependencyNode], String]): String = {
      val buffer = new StringBuffer(4092)
      printDot(sentence, buffer, title, nodeStyle, edgeStyle)
      buffer.toString
    }

    def printDot(sentence: Sent, writer: java.lang.Appendable, title: String) {
      printDot(sentence, writer, title, Map.empty, Map.empty)
    }

    def printDotWithHighlights(sentence: Sent, writer: java.lang.Appendable, title: String, specialNodes: Set[DependencyNode], specialEdges: Set[Edge[DependencyNode]]) {
      val filledNodes = specialNodes zip Stream.continually("style=filled,fillcolor=lightgray")

      val nodeStyle = filledNodes
      val edgeStyle = (specialEdges zip Stream.continually("style=filled")) ++
        ((sentence.dgraph.graph.edges -- specialEdges) zip Stream.continually("style=dotted,color=gray"))

      printDot(sentence, writer, title, nodeStyle.toMap, edgeStyle.toMap)
    }

    def printDot(sentence: Sent, writer: java.lang.Appendable, title: String, nodeStyle: Map[DependencyNode, String], edgeStyle: Map[Edge[DependencyNode], String]) {
      def quote(string: String) = "\"" + string + "\""
      def escape(string: String) = string.replaceAll("\"", "''")
      def nodeString(node: DependencyNode) = {
        val token = sentence.tokens(node.id)
        val text = escape(token.string)
        val postag = escape(token.postag)
        if (sentence.dgraph.graph.vertices.filter(_.string.equals(text)).size > 1)
          text + "_" + postag + "_" + node.id
        else
          text + "_" + postag
      }

      val indent = " " * 2;

      writer.append("digraph g {\n")

      writer.append(indent + "graph [\n")
      writer.append(indent * 2 + "fontname=\"Helvetica-Oblique\"\n")
      writer.append(indent * 2 + "fontsize=\"12\"\n")
      val cleanedTitle = title.replaceAll("\\n", "").replaceAll("\"", "'").replaceAll(";", ",")
      writer.append(indent * 2 + "label=\"" + cleanedTitle + "\"\n")
      writer.append(indent + "]\n\n")

      for (node <- sentence.dgraph.graph.vertices.toSeq.sorted) {
        var parts: List[String] = List()
        if (nodeStyle contains node) {
          parts ::= nodeStyle(node)
        }

        val brackets = "[" + parts.mkString(",") + "]"
        writer.append(indent + quote(nodeString(node)) + " " + brackets + "\n")
      }
      writer.append("\n")

      for (node <- nodeStyle.keys.toSeq.sorted) {
        writer.append(indent + quote(nodeString(node)) + " [" + nodeStyle(node) + "]\n")
      }

      writer.append("\n")
      for (dep <- sentence.dgraph.graph.edges.toSeq.sortBy(edge => (edge.source.id, edge.dest.id, edge.label))) {
        val color = dep.label match {
          case "neg" => Some("red")
          case "amod" | "advmod" => Some("lightblue")
          case "det" | "punct" => Some("lightgrey")
          case "aux" => Some("grey")
          case x if x startsWith "prep" => Some("blue")
          case _ => None
        }

        var parts = List("label=\"" + dep.label + "\"")
        if (color.isDefined) parts ::= "color=\"" + color.get + "\""
        if (edgeStyle.contains(dep)) parts ::= edgeStyle(dep)

        val brackets = "[" + parts.mkString(",") + "]"
        writer.append(indent + quote(nodeString(dep.source)) + " -> " + quote(nodeString(dep.dest)) + " " + brackets + "\n")
      }
      writer.append("}")
    }
  }
}