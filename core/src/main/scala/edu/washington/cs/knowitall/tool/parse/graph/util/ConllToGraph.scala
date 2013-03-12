package edu.washington.cs.knowitall.tool.parse.graph.util

import java.io.PrintWriter
import edu.knowitall.common.Resource.using
import java.io.File
import scala.io.Source
import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph

object ConllToGraph extends App {
  require(args.size <= 2)
  using(args.headOption match {
    case Some(from) => Source.fromFile(from, "UTF-8")
    case None => Source.stdin
  }) { source =>

    using(args.drop(1).headOption match {
      case Some(to) => new PrintWriter(new File(to), "UTF-8")
      case None => new PrintWriter(System.out)
    }) { writer =>
      val lines = source.getLines

      while (!lines.isEmpty) {
        val graph = DependencyGraph.fromCONLL(lines)
        writer.println(graph.serialize)
      }
    }
  }
}
