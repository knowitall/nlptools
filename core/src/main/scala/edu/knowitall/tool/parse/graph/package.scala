package edu.knowitall.tool.parse

import edu.knowitall.collection.immutable.graph.Graph
import edu.knowitall.collection.immutable.graph.Graph.Edge

package object graph {
  type Dependency = Edge[DependencyNode]
}