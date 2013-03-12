package edu.knowitall.tool.srl

import edu.knowitall.tool.parse.graph.DependencyGraph

case class FrameHierarchy(frame: Frame, children: Seq[FrameHierarchy]) {
  def height: Int =
    if (children.isEmpty) 1
    else 1 + children.iterator.map(_.height).max
  override def toString = frame.toString + (if (children.size > 0) (" < " + children.mkString(", ")) else "")
}

object FrameHierarchy {
  def fromFrames(dgraph: DependencyGraph, frames: Seq[Frame]) = {
    if (frames.isEmpty) Seq.empty
    else {
      val framesWithIndex = frames.zipWithIndex

      // find all ancestor -> descendant relationships
      val descendants = framesWithIndex.map {
        case (frame, index) =>
          val inferiors = dgraph.graph.inferiors(frame.relation.node, dedge => !(dedge.source == frame.relation.node && dedge.label == "conj"))
          val children = framesWithIndex.filter {
            case (child, index) =>
              frame != child &&
                // first arguments must match
                frame.argument(Roles.A0) == child.argument(Roles.A0) &&
                // child frame must be beneath parent frame relation in dependency graph
                (inferiors contains child.relation.node)
          }
          index -> children.map(_._2)
      }

      var hierarchies = Map.empty[Int, FrameHierarchy]
      for (i <- Range(0, descendants.iterator.map(_._2.size).max + 1)) {
        val targetDescendants = descendants.filter(_._2.size == i)

        for ((frameIndex, childrenIndices) <- targetDescendants) {
          val frame = frames(frameIndex)
          val children = childrenIndices flatMap hierarchies.get
          hierarchies --= childrenIndices

          hierarchies += frameIndex -> FrameHierarchy(frame, children)
        }
      }

      hierarchies.map(_._2)
    }
  }
}
