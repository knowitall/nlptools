package edu.knowitall.tool.parse

import edu.knowitall.collection.immutable.graph.Graph
import edu.knowitall.collection.immutable.graph.Graph.Edge

package object graph {
  type JoinedDependencyGraph = Graph[JoinedDependencyNode]
  
  type Dependency = Edge[DependencyNode]
}