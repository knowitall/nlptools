package edu.washington.cs.knowitall.tool.srl

import edu.washington.cs.knowitall.tool.parse.graph.DependencyGraph

case class FrameHierarchy(frame: Frame, children: Seq[FrameHierarchy]) {
  override def toString = frame.toString + (if (children.size > 0) (" < " + children.mkString(", ")) else "")
}

object FrameHierarchy {
  def fromFrames(dgraph: DependencyGraph, frames: IndexedSeq[Frame]) = {
    val framesWithIndex = frames.zipWithIndex

    // find all ancestor -> descendant relationships
    val descendants = framesWithIndex.map { case (frame, index) =>
      val inferiors = frame.arguments.flatMap(argument => dgraph.graph.inferiors(argument.node))
      val children = framesWithIndex.filter { case (child, index) => frame != child && (inferiors contains child.relation.node) }
      index -> children.map(_._2)
    }

    var hierarchies = Map.empty[Int, FrameHierarchy]
    for (i <- Range(0, descendants.iterator.map(_._2.size).max + 1)) {
      val targetDescendants = descendants.filter(_._2.size == i)

      for ((frameIndex, childrenIndices) <- targetDescendants) {
        val frame = frames(frameIndex)
        val children = childrenIndices map hierarchies
        hierarchies --= childrenIndices

        hierarchies += frameIndex -> FrameHierarchy(frame, children)
      }
    }

    hierarchies.map(_._2)
  }
}